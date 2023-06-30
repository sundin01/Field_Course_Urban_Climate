extract.rsq <- function(mod, df){
  conflicts_prefer(dplyr::filter)
  df <- df |>drop_na()
  df$fitted <- predict(mod, newdata = df)
  metrics_df <- df |>
    yardstick::metrics(temperature, fitted)
  rmse_df <- metrics_df |>
    filter(.metric == "rsq") |>
    pull(.estimate)
  return(rmse_df)
}

extract.mae <- function(mod, df){
  conflicts_prefer(dplyr::filter)

  df <- df |>drop_na()

  df$fitted <- predict(mod, newdata = df)

  metrics_df <- df |>
    yardstick::metrics(temperature, fitted)

  mae_df <- metrics_df |>
    filter(.metric == "mae") |>
    pull(.estimate)

  return(mae_df)
}

parameter.extracter.knn.cv <- function(own.sequence, df.1, df.2, number.of.validation){
  source("../Field_Course_Urban_Climate/R/ml_functions/knn.cv.model.creater.R")
  add.rsq.1 <- NULL
  add.rsq.2 <- NULL
  add.mae.1 <- NULL
  add.mae.2 <- NULL
  add.k <- NULL
  for (i in own.sequence) {
    # Train the model
    mod.knn <- knn.cv.model(df.1, i, number.of.validation)
    current.k <- i
    current.rsq.1 <- extract.rsq(mod.knn, df.1)
    current.mae.1 <- extract.mae(mod.knn, df.1)
    add.rsq.1 <- c(add.rsq.1, current.rsq.1)
    add.mae.1 <- c(add.mae.1, current.mae.1)
    current.rsq.2 <- extract.rsq(mod.knn, df.2)
    current.mae.2 <- extract.mae(mod.knn, df.2)
    add.rsq.2 <- c(add.rsq.2, current.rsq.2)
    add.mae.2 <- c(add.mae.2, current.mae.2)
    add.k <- c(add.k, i)
  }
  my.tibble <- tibble("K" = add.k,
                      "RSQ_Train" = add.rsq.1,
                      "MAE_Train" = add.mae.1,
                      "RSQ_Test" = add.rsq.2,
                      "MAE_Test" = add.mae.2)

  optimal.k <- as.numeric(my.tibble|>
                            dplyr::filter(`MAE_Test` == min(`MAE_Test`))|>
                            select("K"))

  plot_1 <- my.tibble|>
    pivot_longer(cols = c(my.tibble|>
                            select(starts_with(c("R", "M")))|>
                            colnames()),
                 names_to = "Variable",
                 values_to = "Value")|>
    ggplot(aes(x = `K`, y = `Value`, color = `Variable`)) +
    geom_point() +
    geom_line() +
    geom_point(data = my.tibble, aes(x = optimal.k, y = min(my.tibble$MAE_Test),
                                     color = "Optimal K"), size = 4)+
    labs(x = "# k", y = "RSQ / MAE",
         title = "Overview of the development of RSQ and MAE",
         subtitle = paste("by using KNN-Algorithm,"," Method: cv with n-fold = ",number.of.validation))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    theme_light()
  return(plot_1)
}
