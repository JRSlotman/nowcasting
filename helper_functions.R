RMSE <- function(preds){sqrt(mean((preds$actuals - preds$predictions) ^ 2, na.rm = TRUE))}
mu_fit <- function(preds){sqrt(mean((0 - preds$actuals) ^ 2, na.rm = TRUE))}

remove_outliers_mad <- function(x){if(is.numeric(x)){x[abs(x - median(x))/mad(x, na.rm = TRUE) > 3] <- NA}; return(x)}
remove_outliers_z <- function(x){if(is.numeric(x)){x[abs(x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE) > 3] <- NA};  return(x)}
# sufficient_coverage <-function(x){
#     # Select only features with more than ten observations, and at least one observation 15 years before end date and at least one observation within two years of end date
#     sum(!is.na(x)) > 10 || 
#     min(which(!is.na(x))) < nrow(countrydata)-180 ||
#     max(which(!is.na(x))) > nrow(countrydata)-24
#     }

initialize_data <- function(countrydata, 
                            target, 
                            train_size = 0.7, 
                            test_end_date, 
                            scale_target_tf, 
                            explanatory_variables = NA,
                            parameters){
  
  test_end_date <- as.Date(test_end_date)
  
  target_obs <- countrydata %>% 
    filter(!is.na(!!as.symbol(target)) & date <= test_end_date) %>% 
    select(date) %>% 
    pull()
  
  if(length(target_obs)==0){
    print("The target variable is not available for this country")
    return(NULL)
    }
  
  train_end_date <- target_obs[round(length(target_obs) * train_size, 0)]
  
  foo <- countrydata %>% 
    filter(date <= train_end_date) %>%
    select(-iso) %>% 
    mutate(across(ends_with("_yoy"), ~ifelse(abs(.x) > 200, NA, .x))) %>%
    mutate(across(-all_of(target), remove_outliers_mad)) %>%
    mutate(across(-all_of(target), remove_outliers_z)) %>%
    #select(where(function(x)!all(is.na(x)))) %>% 
    select(where(~sum(!is.na(.x)) > 10)) %>% 
    #mutate(across(contains(c("exchange_rate_yoy", "consumer_credit_yoy", "food_inflation", "private_sector_credit_yoy", "stock_markets_m_yoy", "inflation_expectations", "cpi_a_yoy")), ~replace(., cpi_a_yoy > 50, NA))) %>%
    mutate(across(-all_of(c("date", target)), na.approx, na.rm = FALSE),
          # across(-all_of(c("date", target)), ~replace_na(.x, replace = median(.x, na.rm = TRUE))),
           .keep = "unused") %>%
    relocate(!!sym(target), .after = date)
  
  if (!is.na(explanatory_variables)[1]) {
    foo <- foo %>% 
      select(date, !!as.symbol(target), any_of(explanatory_variables))
  }
  
  test <- foo %>% 
    bind_rows(countrydata[countrydata$date > train_end_date & countrydata$date <= test_end_date, colnames(foo)])
  
  if(scale_target_tf){
    preprocess_values <- preProcess(foo, method = c("nzv", "center", "scale")) # corr
  }else{
    preprocess_values <- preProcess(foo[ , ! colnames(foo) %in% c(target)],
                                    method = c("nzv", "center", "scale")) # Try knnImpute/bagImpute
  }

  train <- stats::predict(preprocess_values, foo)
  
  test <- stats::predict(preprocess_values, test)
  
  parameters[[1]] <- train
  parameters[[2]] <- target

  return (list(train = train, 
               test = test, 
               args = parameters, 
               train_end_date = train_end_date, 
               test_end_date = test_end_date))
}

plot_predictions <- function(train_test_data, predictions){
  y <- predictions[predictions$date > train_test_data$train_end_date, "actuals"]
  yhat <- predictions[predictions$date > train_test_data$train_end_date, "predictions"]

  RMSE <- round(sqrt(mean((y - yhat)**2)), 4)
  MAE <- round(mean(abs(y - yhat)), 4)
  
  predictions %>% 
    gather(label, value, -date) %>%
    filter(!is.na(value)) %>% 
    ggplot() + 
    aes(x=date, y=value, color=label) +
    geom_line() +
    geom_vline(xintercept = train_test_data$train_end_date, colour="darkgrey", linetype = "longdash") +
    scale_x_date(date_breaks = "24 months", date_labels = "%Y") +
    ggtitle(paste0(train_test_data$args$target_variable, " predictions for ", i),
      subtitle = str_interp("Test MAE: ${MAE}, RMSE: ${RMSE}")) +
    theme(
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.9, 0.9),
      legend.background = element_blank()
    )
}

cv_exclude_feature <- function(crossArg, target, feature){
  p <- progressr::progressor(steps = nrow(crossArg))
  
  map2_df(crossArg$x, crossArg$y, possibly(otherwise = NULL,
                                           function(i, prm_value){
                                             p()
                                             stopwatch <- Sys.time()
                                             
                                             if(prm_value){
                                               countrydata <- data[data$iso == i,] %>% select(-!!as.symbol(feature))
                                             }else{
                                               countrydata <- data[data$iso == i,] 
                                             }
                                             
                                             train_test_data <- initialize_data(countrydata,
                                                                                target = target,
                                                                                train_size = 0.7, 
                                                                                test_end_date = "2019-12-1", 
                                                                                scale_target_tf = T, 
                                                                                explanatory_variables = NA,
                                                                                parameters = fast) 
                                             
                                             model <- do.call(LSTM, args = train_test_data$args)
                                             
                                             predictions <- ragged_preds(model, 
                                                                         data = train_test_data$test, 
                                                                         lag = -3, # Date of prediction - date of target (e.g. predict Jun in Apr -> -2)
                                                                         pub_lags = rep(-2, ncol(train_test_data$test)-2)) # Lag from target date of input data (e.g. predict Jun with input data as late as Apr -> -2) 
                                             
                                             #print(RMSE(predictions[predictions$date > train_test_data$train_end_date & predictions$date <= train_test_data$test_end_date,]))
                                             
                                             tibble(country = i,
                                                    prm_value = ifelse(prm_value,paste("Without", feature),paste("With", feature)),
                                                    RMSE_test = RMSE(predictions[predictions$date > train_test_data$train_end_date & predictions$date <= train_test_data$test_end_date,]),
                                                    RMSE_train =  RMSE(predictions[predictions$date <= train_test_data$train_end_date,]),
                                                    runtime = Sys.time()-stopwatch)
                                           }))
  }

cv_grid_search <- function(crossArg, target, parameter, tunegrid, features){
  p <- progressr::progressor(steps = nrow(crossArg))
  
  map2_df(crossArg$x, crossArg$y, possibly(otherwise = NULL,
                                           function(i, prm_value){
                                             p()
                                             stopwatch <- Sys.time()
                                             
                                             tunegrid[parameter] <- prm_value
                                             train_test_data <- initialize_data(data[data$iso == i,] ,
                                                                                target = target,
                                                                                train_size = 0.7, 
                                                                                test_end_date = "2019-12-1", 
                                                                                scale_target_tf = T, 
                                                                                explanatory_variables = features,
                                                                                parameters = tunegrid) 
                                             
                                             model <- do.call(LSTM, args = train_test_data$args)
                                             
                                             predictions <- ragged_preds(model, 
                                                                         data = train_test_data$test, 
                                                                         lag = -3, # Date of prediction - date of target (e.g. predict Jun in Apr -> -2)
                                                                         pub_lags = rep(-2, ncol(train_test_data$test)-2)) # Lag from target date of input data (e.g. predict Jun with input data as late as Apr -> -2) 

                                             tibble(country = i,
                                                    prm_value = prm_value,
                                                    RMSE_test = RMSE(predictions[predictions$date > train_test_data$train_end_date & predictions$date <= train_test_data$test_end_date,]),
                                                    RMSE_train =  RMSE(predictions[predictions$date <= train_test_data$train_end_date,]),
                                                    runtime = Sys.time()-stopwatch)
                                           }))
}

cv_nowcast <- function(crossArg, target, periods){
  p <- progressr::progressor(steps = nrow(crossArg))
  
  map2_df(crossArg$x, crossArg$y, possibly(otherwise = NULL,
                                           function(i, prm_value){
                                             p()
                                             stopwatch <- Sys.time()
                                             
                                             train_test_data <- initialize_data(data[data$iso == i,],
                                                                                target = target,
                                                                                train_size = 0.7, 
                                                                                test_end_date = "2019-12-1", 
                                                                                scale_target_tf = T, 
                                                                                explanatory_variables = NA,
                                                                                parameters = fast) 
                                             
                                             model <- do.call(LSTM, args = train_test_data$args)
                                             
                                             predictions <- ragged_preds(model, 
                                                                         data = train_test_data$test, 
                                                                         lag = -3, # Date of prediction - date of target (e.g. predict Jun in Apr -> -2)
                                                                         pub_lags = rep(-2, ncol(train_test_data$test)-2)) # Lag from target date of input data (e.g. predict Jun with input data as late as Apr -> -2) 
                                             
                                            if(prm_value){
                                              predictions$predictions <- (predictions$predictions/lag(predictions$predictions, periods))*lag(predictions$actuals, periods)
                                            }                                        
                                             tibble(country = i,
                                                    prm_value = ifelse(prm_value,"Incorporating lagged actual","Regular prediction"),
                                                    RMSE_test = RMSE(predictions[predictions$date > train_test_data$train_end_date & predictions$date <= train_test_data$test_end_date,]),
                                                    RMSE_train =  RMSE(predictions[predictions$date <= train_test_data$train_end_date,]),
                                                    runtime = Sys.time()-stopwatch)
                                           }))
}


