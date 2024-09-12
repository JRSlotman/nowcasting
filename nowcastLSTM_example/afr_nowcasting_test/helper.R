### initial reading of data, start_date = earliest date in data (in str format), end_date = latest date in data (in str format)
initialize_data <- function (start_date, end_date) {
  # creating a sequence of dates in the data
  start_date <- as.Date(start_date) # earliest start date in the data
  end_date <- as.Date(end_date) # latest date in the data
  data <- seq(start_date, end_date, by = "1 month") %>% 
    tibble()
  colnames(data) <- "date"
  
  # combine all files into one
  for (file_path in list.files(".")) {
    if (grepl(".csv", file_path)) {
      tmp <- read_csv(file_path)
      iso <- tmp[1,1] %>% pull()
      tmp <- tmp %>% 
        select(-iso)
      
      # append iso to column names to be able to have all columns in one file
      colnames(tmp)[2:ncol(tmp)] <- paste0(iso, "_", colnames(tmp)[2:ncol(tmp)])
      
      # joining on the 'data' dataframe
      data <- data %>% 
        left_join(tmp, by="date")
    }
  }
  
  # convert everything into float, not percentage point
  for (column in colnames(data)[2:ncol(data)]) {
    data[,column] <- data[,column] / 100
  }
  
  return (data)
}

### set up train/test data
set_up_data <- function (data, target_variable, train_size = 0.8, explanatory_variables = NA) {
  # only use given explanatory variables, default to using all
  if (!is.na(explanatory_variables)[1]) {
    data <- data %>% 
      select(date, !!as.symbol(target_variable), explanatory_variables)
  }
  
  target_obs <- data %>% 
    filter(!is.na(!!as.symbol(target_variable))) %>% 
    select(date) %>% 
    pull()
  
  train_end_date <- target_obs[round(length(target_obs) * train_size, 0)]
  
  # drop columns with no data in the train period
  for (column in colnames(data)[2:ncol(data)]) {
    tmp <- data %>% 
      filter(date <= train_end_date)
    if (sum(is.na(tmp[,column])) == nrow(tmp)) {
      data <- data %>% 
        select(-!!as.symbol(column))
    }
  }
  
  # train and test data
  train <- data %>% 
    filter(date <= train_end_date)
  
  test <- tibble(data)
  
  return (list(train = train, test = test, train_end_date = train_end_date))
}

### visualize a trained model, test = test dataset, train_end_date = latest data for training
visualize_model <- function (model, train_end_date, test) {
  # prediction dataframe
  preds <- predict(model, test, only_actuals_obs = TRUE) 
  
  # calculating RMSE
  actuals <- preds %>% 
    filter(date > train_end_date) %>% 
    select(actuals) %>% 
    pull()
  
  predictions <- preds %>% 
    filter(date > train_end_date) %>% 
    select(predictions) %>% 
    pull()
  
  RMSE <- round(sqrt(mean((actuals - predictions)**2)), 4)
  MAE <- round(mean(abs(actuals - predictions)), 4)
  
  # getting in form form plotting
  preds <- preds %>% 
    gather(series, value, -date)
  
  preds %>% 
    ggplot() + 
    aes(x = date, y = value, color = series) +
    geom_line() +
    geom_vline(xintercept = as.numeric(train_end_date)) +
    ggtitle(str_interp("test MAE: ${MAE}, RMSE: ${RMSE}"))
}