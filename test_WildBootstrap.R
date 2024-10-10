library(fwildclusterboot)
boot <- boottest(didreg,
                 B = 999,
                 param = "DID", 
                 clustid = "DPTO",
                 type = "rademacher"
) # for reproducibility

# Print the bootstrap p-values
summary(boot)
print(boot)
plot(boot)
nobs(boot)
pval(boot)
confint(boot)
print(boot$p_val)
print(boot$conf_int[1])
print(boot$conf_int[2])
print(boot$t_stat)
print(boot$engine)


# Define a function to run boottest.lm on each predictor
run_boottest_on_all_vars <- function(didreg, cluster_var, B = 500, type = "rademacher") {
  # Extract predictor names
  predictors <- names(coef(didreg))[-1]  # Exclude intercept
  
  # Store results
  results <- list()
  
  # Perform wild bootstrap tests for each predictor
  for (predictor in predictors) {
    cat("Testing:", predictor, "\n")
    test_result <- boottest(didreg,
                            clustid = as.formula(paste("~", cluster_var)),
                            param = predictor,
                            B = B,
                            type = type)
    results[[predictor]] <- test_result
  }
  
  return(results)
}

# Run the function
cluster_var <- "DPTO"  # Clustering variable name
bootstrap_results <- run_boottest_on_all_vars(didreg, cluster_var, B = 500, type = "rademacher")

# Print results
for (predictor in names(bootstrap_results)) {
  
  cat("Results for", predictor, ":\n")
  print(bootstrap_results[[predictor]])
  cat("\n")
}