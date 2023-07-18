# function to find the best model fit (step forward algorithm)

# The function needs a data.frame and the column number of the target.
multivariate.reg.model <- function(dataframe, vector){
  # Read all columns but the target and save their names in a vector
  col.names <- dataframe|>
    select(-vector) |>
    colnames()
  # Start with a empty vector. Here we will write down the predictors of the best fit for each round
  predictors <- NULL
  # Start with a empty vector for the column names. Here we will write down the fittest predictor for each round
  column.name <- NULL
  collect.predictors <- NULL
  collect.RR <- NULL
  collect.AIC <- NULL
  # Start with a big number. While loop should run till the AIC become bigger again
  AIC.old <- 9999999
  AIC.new <- 9999998 # We also could control the while with TRUE nd if-else statement.
  # But never change a function which work perfectly...

  # Start the while-loop
  while (AIC.new < AIC.old) {
    # Change the watcher for the while loop. We change the watcher for each round
    AIC.old <- AIC.new
    # Set the RR value to zero. We want a new start for each round in the for loop
    RR.now <- 0
    # col.names will be shorter for each round because we delete always the best fit from the vector
    for (i in col.names) {
      # For each round, we create a new formula for the linear regression model.
      changeable.formula <- as.formula(paste("temperature~", paste(c(predictors, i), collapse = "+")))
      # Calculate the linear regression model
      lm.model <- lm(changeable.formula, data = dataframe)
      # Read R^2
      RR.new <- summary(lm.model)$r.squared
      # Search the highest R square and save the column name. We have to delete the column for the next round
      if(RR.new > RR.now){
        # Change the guard to enter the if condition
        RR.now <- RR.new
        # Extract the AIC. If the AIC bigger in the next round, we will overwrite it.
        AIC.new <- extractAIC(lm.model)[2]
        # We read the column name. If the AIC bigger for another predictor, we will overwrite it
        column.name <- as.character(i)

      } # end if condition

      # else does nothing
      else{NULL
      } # end else condition

    }# end for loop

    # Delete the name of the best fit of this round from the vector
    col.names <- col.names[!col.names == column.name]

    # prepare the predictors for the next round
    predictors <- c(predictors, column.name)
    predictors.tibble <- predictors
    collect.RR <- c(collect.RR, RR.now)
    collect.AIC <- c(collect.AIC, AIC.new)

  }# end while loop

  # if we finished the while loop, we print our model
  print(paste("Best model fit has AIC =", AIC.old))
  predictors <- predictors[-length(predictors)]
  print(predictors)
  my.tibble <- tibble::tibble("Predictors" = predictors.tibble,
                              "RSQ" = collect.RR,
                              "AIC" = collect.AIC)
  return(my.tibble)
}
