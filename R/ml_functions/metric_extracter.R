
rmse_extracter <- function(mod, df_train, df_test, train = FALSE){
  # add predictions to the data frames
  df_train <- df_train |>
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  df_train$bias <- df_train$temperature - df_train$fitted

  df_test <- df_test |>
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  df_test$bias <- df_test$temperature - df_test$fitted

  # get metrics tables
  metrics_train <- df_train |>
    yardstick::metrics(temperature, fitted)

  metrics_test <- df_test |>
    yardstick::metrics(temperature, fitted)

  if(train == TRUE){
    rmse_train <- round(metrics_train |>
                          filter(.metric == "rmse") |>
                          pull(.estimate), digits = 2)
    return(rmse_train)
  }else{
    rmse_test <- round(metrics_test |>
                         filter(.metric == "rmse") |>
                         pull(.estimate), digits = 2)
    return(rmse_test)}
}

rsq_extracter <- function(mod, df_train, df_test, train = FALSE){
  # add predictions to the data frames
  df_train <- df_train |>
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  df_train$bias <- df_train$temperature - df_train$fitted

  df_test <- df_test |>
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  df_test$bias <- df_test$temperature - df_test$fitted

  # get metrics tables
  metrics_train <- df_train |>
    yardstick::metrics(temperature, fitted)

  metrics_test <- df_test |>
    yardstick::metrics(temperature, fitted)

  if(train == TRUE){
    rsq_train <- round(metrics_train |>
                         filter(.metric == "rsq") |>
                         pull(.estimate), digits = 2)
    return(rsq_train)
  }else{
    rsq_test <- round(metrics_test |>
                        filter(.metric == "rsq") |>
                        pull(.estimate), digits = 2)
    return(rsq_test)}
}

bias_extracter <- function(mod, df_train, df_test,train = FALSE){
  # add predictions to the data frames
  df_train <- df_train |>
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  df_train$bias <- df_train$temperature - df_train$fitted

  df_test <- df_test |>
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  df_test$bias <- df_test$temperature - df_test$fitted

  # get metrics tables
  metrics_train <- df_train |>
    yardstick::metrics(temperature, fitted)

  metrics_test <- df_test |>
    yardstick::metrics(temperature, fitted)

  if(train == TRUE){
    bias_train <- round(sum(df_train$bias, na.rm = TRUE)/length(df_train$bias), digits = 2)
    return(bias_train)
  }else{
    bias_test <- round(sum(df_test$bias, na.rm = TRUE)/length(df_test$bias), digits = 2)
    return(bias_test)}
}


