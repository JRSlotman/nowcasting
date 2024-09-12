if("librarian" %in% rownames(installed.packages()) == FALSE) {install.packages("librarian")}
librarian::shelf(here, tidyverse, dhopp1/nowcastLSTM)
source("helper.R")
#renv::use_python(paste0(miniconda_path(),"/python.exe"))

nowcastLSTM::initialize_session(paste0(miniconda_path(),"/python.exe"))

### data read
data <- initialize_data(start_date = "1992-12-01", end_date = "2023-03-01")

# which variables have enough data to do validation (so training data at least starting 60% of 80%)
target_variable <- "ETH_gdp_a_yoy"
feasible_explanatory_variables <- set_up_data(data, target_variable, train_size = 0.8*0.6) %>% 
  .$test %>% 
  select(-date) %>% 
  colnames()

### variable selection step
if (T) {
  data_list <- set_up_data(data, target_variable, train_size = 0.8, feasible_explanatory_variables)
  train_end_date <- data_list$train_end_date
  train <- data_list$train
  test <- data_list$test
  
  start <- Sys.time()
  selected_variables <- variable_selection(
    data = train,
    target_variable = target_variable,
    n_timesteps = 12,
    n_models = 1,
    train_episodes = 10,
    batch_size = round(((train %>% filter(!is.na(!!as.symbol(target_variable)))) %>% nrow()) * 0.25, 0), # number of target series observations * coefficient,
    n_hidden = 5,
    n_layers = 2,
    n_folds = 1,
    init_test_size = 0.2,
    initial_ordering = "feature_contribution",
    pub_lags = -1,
    quiet = FALSE
  )
  beep(sound = 1, print(Sys.time()-start))
  
  selected_variables <- selected_variables$col_names
  print(selected_variables)
} else {
  # from variable selection function
  #selected_variables <- c("NGA_stock_markets_yoy", "NGA_exchange_rate_yoy", "NGA_gas_price_yoy_fit", "ETH_gas_price_yoy_fit", "ZAF_coincident_index_yoy", "NGA_msci_yoy", "ETH_copper_price_yoy_fit", "NGA_commodity_price_index_yoy_fit", "ZAF_food_price_yoy_fit", "ZAF_core_cpi_m_yoy", "NGA_undernourishment_yoy")
  # from feature contribution, top 25
  selected_variables <- c("NGA_stock_markets_yoy", "ETH_copper_price_yoy", "NGA_copper_price_yoy", "NGA_exchange_rate_yoy", "ZAF_copper_price_yoy", "ETH_gas_price_yoy", "NGA_gas_price_yoy", "ZAF_stock_markets_yoy", "ZAF_global_economic_activity", "NGA_global_economic_activity", "ZAF_brent_price_yoy", "ZAF_gas_price_yoy", "ZAF_exchange_rate_yoy", "ETH_global_economic_activity", "ZAF_wheat_price_yoy", "NGA_wheat_price_yoy", "ETH_cpi_a_yoy", "ETH_wheat_price_yoy", "ETH_brent_price_yoy", "ZAF_nitro_fertilizer_price_yoy", "NGA_brent_price_yoy", "ETH_nitro_fertilizer_price_yoy", "ETH_global_economic_activity_fit", "NGA_nitro_fertilizer_price_yoy", "NGA_msci_yoy")
}

### hyperparameter tuning step
if (F) {
  data_list <- set_up_data(data, target_variable, train_size = 0.8, selected_variables)#, selected_variables)
  train_end_date <- data_list$train_end_date
  train <- data_list$train
  test <- data_list$test
  n_obs <- ((train %>% filter(!is.na(!!as.symbol(target_variable)))) %>% nrow())
  
  tuning <- hyperparameter_tuning(
    data = train,
    target_variable = target_variable,
    n_models = 5,
    n_timesteps_grid = c(6, 12), # maybe also add 9
    train_episodes_grid = c(50, 100, 200),
    batch_size_grid = c(round(n_obs * 0.25, 0), round(n_obs * 0.5, 0), round(n_obs * 0.75, 0)),
    n_hidden_grid = c(10, 20, 30),
    n_layers_grid = c(1, 2, 4),
    n_folds = 2,
    init_test_size = 0.4
  )
  tuning$hyper_params <- tuning$hyper_params %>% paste0()
  write_csv(tuning, "tuning_results.csv")
}

### model selection (hyperparam tuning + variable selection combined)
if (F) {
  data_list <- set_up_data(data, target_variable, train_size = 0.8, feasible_explanatory_variables)#, selected_variables)
  train_end_date <- data_list$train_end_date
  train <- data_list$train
  test <- data_list$test
  n_obs <- ((train %>% filter(!is.na(!!as.symbol(target_variable)))) %>% nrow())
  
  selected_model <- select_model(
    data = train,
    target_variable = target_variable,
    n_models = 5,
    n_timesteps_grid = c(6,9), # maybe also add 9
    train_episodes_grid = c(50),
    batch_size_grid = c(round(n_obs * 0.5, 0)),
    n_hidden_grid = c(10),
    n_layers_grid = c(1, 2),
    n_folds = 2,
    init_test_size = 0.4
  )
  
  selected_model$hyperparameters <- selected_model$hyperparameters %>% paste0()
  selected_model$variables <- selected_model$variables %>% paste0()
  write_csv(selected_model, "model_selection_results.csv")
}

### visualizing final model
if (F) {
  data_list <- set_up_data(data, target_variable, train_size = 0.8, selected_variables)
  train_end_date <- data_list$train_end_date
  train <- data_list$train
  test <- data_list$test
  
  # hyperparameters gotten from tuning results CSV
  model <- LSTM(
    train,
    target_variable,
    n_timesteps = 6,
    n_models = 50,
    train_episodes = 50,
    batch_size = 12, # number of target series observations * coefficient
    n_hidden = 10,
    n_layers = 4
  )
  
  visualize_model(model, train_end_date, test) 
  ggsave("final_model.png", height = 5, width = 10)
}