eval_model.a <- function(mod, df_train, df_test, model.name.train, model.name.test){
  conflicts_prefer(dplyr::filter)

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

  # extract values from metrics tables
  rmse_train <- round(metrics_train |>
                        filter(.metric == "rmse") |>
                        pull(.estimate), digits = 2)

  rsq_train <- round(metrics_train |>
                       filter(.metric == "rsq") |>
                       pull(.estimate), digits = 2)

  rmse_test <- round(metrics_test |>
                       filter(.metric == "rmse") |>
                       pull(.estimate), digits = 2)

  rsq_test <- round(metrics_test |>
                      filter(.metric == "rsq") |>
                      pull(.estimate), digits = 2)

  bias.train <- round(sum(df_train$bias, na.rm = TRUE)/length(df_train$bias), digits = 2)
  bias.test <- round(sum(df_test$bias, na.rm = TRUE)/length(df_test$bias), digits = 2)

  # visualize as a scatter plot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(temperature, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotdash",
                linewidth = 0.5, color = "orange") +
    labs(subtitle = paste("RSQ = ",rsq_train,", RMSE = ",rmse_train,", Bias = ",bias.train),
         title = paste("Train:", model.name.train),
         x = expression(paste("observed Temperature [°C]")),
         y = expression(paste("fitted Temperature [°C]"))) +
    theme_light()

  plot_2 <- ggplot(data = df_test, aes(temperature, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotdash",
                linewidth = 0.5, color = "orange") +
    labs(subtitle = paste("RSQ = ",rsq_test,", RMSE = ",rmse_test,", Bias = ",bias.test),
         title = paste("Test:", model.name.test),
         x = expression(paste("observed Temperature [°C]")),
         y = expression(paste("fitted Temperature [°C]"))) +
    theme_light()

  out <- cowplot::plot_grid(plot_1, plot_2)

  return(out)
}
