knn.cv.model <- function(database_train, number.of.k = 8, number.of.validation = 10){
  # Model and pre-processing formulation, use all variables but LW_IN_F
  pp <- recipes::recipe(temperature ~ humidity,
                        data = database_train |> drop_na()) |>
    recipes::step_BoxCox(all_predictors()) |>
    recipes::step_center(all_numeric(), -all_outcomes()) |>
    recipes::step_scale(all_numeric(), -all_outcomes())

  # us cv (cross validation) as method
  mod_cv <- caret::train(pp,data = database_train |> drop_na(),
                         method = "knn",
                         trControl = caret::trainControl(method = "cv",
                                                         number = number.of.validation),
                         tuneGrid = data.frame(k = number.of.k),
                         metric = "MAE")
  return(mod_cv)
}
