cluster_se <- vcovCL(REGRESSION, cluster = ~ DPTO)

REGRESSION_cluster_se<-coeftest(REGRESSION,cluster_se)
print(REGRESSION_cluster_se)


# Alternatively, to create a table manually
result_table <- data.frame(
  term = rownames(REGRESSION_cluster_se),
  Estimate = REGRESSION_cluster_se[, "Estimate"],
  `Std. Error` = REGRESSION_cluster_se[, "Std. Error"],
  `t value` = REGRESSION_cluster_se[, "t value"]
)

result_table <- result_table %>%
  rename(StdERROR = Std..Error, tVALUE = t.value)
result_table <- result_table %>% filter(term %in% params)
