# Set a seed for reproducibility
cluster_var <- "DPTO"
# Define a function to perform boottest and construct a tidy data frame
perform_boottest <- function(REGRESSION, cluster_var, B = 999, type = "rademacher") {

  # List of parameters to test
  #params<-names(coef(REGRESSION))
  params<-params
  # Initialize a list to store results
  results <- list()
  
  # Loop through each parameter
  for (param in params) {
    # Perform boottest for each parameter
    test_result <- boottest(REGRESSION,
                               clustid = as.formula(paste("~", cluster_var)),
                               param = param,
                               B = B,
                               type = type)
    
    # Extract results
    test_stat <- test_result$t_stat
    p_value <- test_result$p_val
    conf_int <- test_result$conf_int
    
    # Store results in a list
    results[[param]] <- tibble(
      term = param,
      pvalue = p_value,
      conflow = conf_int[1],
      confhigh =conf_int[2],
      estimate = coef(REGRESSION)[param],
      statistic = test_stat
    )
  }
  
  # Combine all results into one data frame
  results_df <- bind_rows(results)
  return(results_df)
}

# Perform boottest on all parameters
results_df <- perform_boottest(REGRESSION, cluster_var, B = 999, type = "rademacher")

