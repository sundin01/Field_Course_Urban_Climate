# Function to determine the best fit for a bivariate regression model
model.fitter <- function(dataframe, target.column.nr){
  
  # Write all column-names into a vector
  col.names <- c(colnames(dataframe))
  
  # Create two empty vectors. We will fill them with the RR for each model
  my.vec <- c()
  aic.vec <- c()
  
  # We create a For-loop over all columns
  for (i in c(1 : ncol(dataframe))) {
    
    # If the target and the predictor are the same,then the RR will be 1.
    # Therefore we use a if statement because we want not to use the target as a predictor
    if (i != target.column.nr){
      lm.model <- lm(unlist(dataframe[target.column.nr]) ~ unlist(dataframe[i]))
      RR <- summary(lm.model)$r.squared
      aic.vec <- c(aic.vec, extractAIC(lm.model)[2])
      my.vec <- c(my.vec, RR )
    }
    # If the target and the predictor are the same, than we want RR = AIC = NA
    else{
      my.vec <- c(my.vec, NA)
      aic.vec <- c(aic.vec, NA)
    }
  }
  
  # We create a tibble for a proper overview
  my.tibble <- tibble("Target" = rep(col.names[target.column.nr],
                                     times = ncol(dataframe)),
                      "Predictor" = col.names,
                      "RR"  = my.vec,
                      "AIC" = aic.vec,
                      "Fit" = ifelse(RR >= max(RR, na.rm = TRUE), "BEST FIT", "NO"))
  # We return our tibble
  return(my.tibble)
}
