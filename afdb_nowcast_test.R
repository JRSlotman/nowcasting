#### PREAMBLE ####
options(future.globals.maxSize= 891289600, future.rng.onMisuse = "ignore")
if("librarian" %in% rownames(installed.packages()) == FALSE) {install.packages("librarian")}
librarian::shelf(here, tidyverse, dhopp1/nowcastLSTM, beepr, furrr, corrr, progressr, caret) 
# Note that some required sub-dependencies may need to be updated for packages to work. This will return a warning. If this is the case, it is best to restart the session and install the packages mentioned in the warning separately.

future::plan(multisession, workers = max(availableCores(),1)) # Helps to speed up certain processes

nowcastLSTM::initialize_session(paste(miniconda_path(),"/python.exe", sep = "")) # No need to worry about warnings

#### SET PARAMETERS AND DATA ####

source("afdb_nowcast_load_data.R")
source("helper_functions.R")

fast <- list(
  data = "",
  target_variable= "",
  n_timesteps=12, # 12 is most accurate but 6 is fastest
  n_models = 1, # Higher is more accurate but 1 is fastest
  train_episodes = 10, # 100 is most accurate but 10 is fastest 
  batch_size = 64, # 10 is most accurate but 100 is fastest
  decay = 0.995, # most accurate, no impact on runtime
  n_hidden = 2, # 36 is most accurate but 1 is fastest
  n_layers = 2, # 2 is most accurate and fastest
  dropout = 0.23, # not much variation in runtime
  optimizer = "torch.optim.RMSprop", # SGD is fastest; RMSprop most accurate 
  criterion = "torch.nn.MSELoss()" # L1Loss is fastest; MSELoss most accurate 
)

target_list <- c("gdp_a_yoy","food_price_yoy", "undernourishment_yoy")
target <- target_list[2]
#### BASIC MODEL ####
i <- "EGY"
train_test_data <- initialize_data(countrydata = data[data$iso == i,],
                             target = target, #gdp_a_yoy food_price_yoy undernourishment_yoy
                             train_size = 0.7, 
                             test_end_date = "2019-12-1", 
                             scale_target_tf = T, 
                             explanatory_variables = tune_variables$col_names, # tune_variables$col_names edge[edge$monthdiff <= 0,]$name
                             parameters = fast)

start <- Sys.time()
model <- do.call(LSTM, args = train_test_data$args)
beep(sound = 1, print(Sys.time()-start))
#save_lstm(model, here("Analysis/Models", paste(today(), i,"feature_contribution.pkl", sep = "_")))

edge <- data[data$iso == i,colnames(train_test_data$test)] %>% 
  pivot_longer(-date) %>% 
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  summarize(last_obs = tail(date, 1)) 

edge$monthdiff <- interval(edge$last_obs, edge[edge$name == target,"last_obs"] %>% pull()) %/% months(1)

predictions <- ragged_preds(model, 
                          data = train_test_data$test, 
                          lag = -3, # Date of prediction - date of target (e.g. predict Jun in Apr -> -2)
                          pub_lags = rep(-2, ncol(train_test_data$test)-2)) # Lag from target date of input data (e.g. predict Jun with input data as late as Apr -> -2) 

#predictions <- predict(model, train_test_data$test, only_actuals_obs=T) # only_actuals_obs=True to only get predictions where there are actuals present
RMSE(predictions[predictions$date > train_test_data$train_end_date & predictions$date <= train_test_data$test_end_date,])
mu_fit(predictions[predictions$date > train_test_data$train_end_date & predictions$date <= train_test_data$test_end_date,])

predictions$predictions_2 <- lag(predictions$actuals,1) + (predictions$predictions-lag(predictions$predictions,1)) # lag(predictions$actuals,1)*(predictions$predictions/lag(predictions$predictions,1))
plot_predictions(train_test_data, predictions)

#### Variable selection ####

model$feature_contribution()

tune_variables <- do.call(variable_selection, 
                          args = c(train_test_data$args, 
                                   init_test_size = 0.3, 
                                   n_fold = 4, 
                                   initial_ordering = "feature_contribution",
                                   #initial_ordering = "univariate",
                                   pub_lags = -1
                          )
)
tune_variables

contribution_scores <- map_df(1:20, 
                              function(i){
                                          model <- do.call(LSTM, args = train_test_data$args)
                                          model$feature_contribution()
                                          }
)

contribution_scores %>% 
  mutate(scaled_contribution = abs(scaled_contribution),
         source = ifelse(feature %in% colnames(fred), "FRED", "Other")) %>% 
  ggplot(aes(x = reorder(feature,scaled_contribution, median), y = scaled_contribution, color = source)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("") + 
  ylab("") 

selected_variables <- contribution_scores %>% 
  group_by(feature) %>% 
  summarise(median_contribution = median(abs(scaled_contribution))) %>% 
              filter(median_contribution > 0.2) %>%
  pull(feature)

#### Hyperparameter tuning ####

n_obs <- ((train_test_data$train %>% filter(!is.na(!!as.symbol(train_test_data$args$target_variable)))) %>% nrow())
tune_sequence <- seq(1, 20, 1)
start <- Sys.time()
tune_hyperparameters <- hyperparameter_tuning(train_test_data$train, 
                                              train_test_data$args$target_variable,
                                              #n_timesteps=12,
                                              n_timesteps_grid = seq(3, 24, 3),
                                              #train_episodes = 10, 
                                              train_episodes_grid = seq(50, 300, 50), 
                                              batch_size = round(n_obs * 0.1, 0), # 10 is most accurate but 100 is fastest
                                              #batch_size_grid = c(round(n_obs * 0.1, 0), round(n_obs * 0.25, 0), round(n_obs * 0.5, 0), round(n_obs * 0.75, 0)),
                                              decay = 0.95, # most accurate, no impact on runtime
                                              #decay_grid = tune_sequence,
                                              n_hidden = 1, # 36 is most accurate but 1 is fastest
                                              #n_hidden_grid = seq(1, 40, 1),
                                              n_layers = 2, # 2 is most accurate and fastest
                                              #n_layers_grid = seq(1, 4, 1),
                                              dropout = 0.1 # not much variation in runtime
                                             # dropout_grid = seq(0.02, 0.3, 0.02)
                                              
             )
qplot(x = tune_sequence , y = tune_hyperparameters$performance)
beep(sound = 1, print(Sys.time()-start))

max_performance <- select_model(traintransformed, output, n_timesteps=12)

#### Cross-validation ####

##### Exclude feature #####

features <- data[data$iso == i,colnames(train_test_data$test)] %>% 
  pivot_longer(-date) %>% 
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  summarise(first = min(date), last = max(date), N = n())

crossArg <- cross_df(list(x = "EGY", #c("CIV", "DZA", "ZAF", "EGY", "MAR", "KEN", "ETH", "NGA"), 
                          y = c(rep(T,10),rep(F,10))))

results <- with_progress({cv_exclude_feature(crossArg, 
                                             target = "food_price_yoy", 
                                             feature = "industrial_production_yoy")}, 
                                          handlers("rstudio"))

results %>% ggplot(aes(country, RMSE_test, colour = prm_value)) + geom_boxplot()
results %>% write_csv(here("Analysis/results.csv"))

##### Hyperparameter tuning #####

tunegrid <- list(
  data = "",
  target_variable= "",
  n_timesteps=12, # 12 is most accurate but 6 is fastest
  n_models = 1, # Higher is more accurate but 1 is fastest
  train_episodes = 10, # 175 is most accurate but 5 is fastest 
  batch_size = 64, # 10 is most accurate but 100 is fastest
  decay = 0.995, # most accurate, no impact on runtime
  n_hidden = 2, # 36 is most accurate but 1 is fastest
  n_layers = 2, # 2 is most accurate and fastest
  dropout = 0.23, # not much variation in runtime
  optimizer = "torch.optim.RMSprop", # SGD is fastest; RMSprop most accurate 
  criterion = "torch.nn.MSELoss()" # L1Loss is fastest; MSELoss most accurate 
)

tunesequence <- c(10, 50, 100, 150, 200, 250, 300) #seq(10,100, 10)
crossArg <- cross_df(list(x = "EGY", #c("CIV", "DZA", "ZAF", "EGY", "MAR", "KEN", "ETH", "NGA"), 
                          y = rep(tunesequence,5)))

results <- with_progress({cv_grid_search(crossArg, 
                                             target = "food_price_yoy", 
                                             parameter = "train_episodes",
                                             tunegrid = tunegrid,
                                         features = tune_variables$col_names  # tune_variables$col_names edge[edge$monthdiff <= 0,]$name
                                         )}, 
                         handlers("rstudio"))

results %>% 
  pivot_longer(cols = c("RMSE_test", "RMSE_train"), names_to = "split", values_to = "RMSE") %>% 
  group_by(country, split, prm_value) %>% 
  summarise(RMSE = median(RMSE)) %>% 
  ggplot(aes(prm_value, RMSE, colour = split)) + geom_point() + facet_wrap(vars(country))
ggsave(here("Analysis/Tuning","episodes_food_price.png"), width = 6, height = 4, dpi=700)

##### Incorporating lagged actual #####

crossArg <- cross_df(list(x = c("CIV", "DZA", "ZAF", "EGY", "MAR", "KEN", "ETH", "NGA"), 
                          y = c(rep(T,10),rep(F,10))))
results <- with_progress({cv_nowcast(crossArg, 
                                         target = target_list[3], 
                                         periods = 1

)}, 
handlers("rstudio"))

results %>% ggplot(aes(country, RMSE_test, colour = prm_value)) + geom_boxplot()
results %>% write_csv(here("Analysis/food_price_nowcast.csv"))
ggsave(here("Analysis","food_price_nowcast.png"), width = 6, height = 4, dpi=700)
