generate_performance_metrics <- function(weights, stocks_preselected = TRUE, returns) {
  
  dates_testing <- sort(data_predef_stocks$dates)[first_test:end_test]
  FF_factors_test <- FF_factors %>% filter(date %in% dates_testing)
  test_smpl_returns <- as.matrix(returns[first_test:end_test, ])
  
  if (stocks_preselected==FALSE) {
    test_smpl_returns <- replace(test_smpl_returns, is.na(test_smpl_returns), 0)
    
  } 
  weights <- do.call(rbind, lapply(weights, unlist))
  pf_returns <- rowSums(weights*test_smpl_returns)
  pf_ret_excess <- pf_returns
  avg_return <- mean(pf_returns)
  sd_return <- sd(pf_returns)
  sharpe_ratio <- mean(pf_ret_excess) / sd_return
  max_weight <- max(weights)
  min_weight <- min(weights)
  avg_neg_weights <- mean(weights[weights < 0])
  frac_neg_weights <- mean(apply(weights, 1, function(row) sum(row[row < 0])))
  FF_factors_test$pf_ret_excess <- pf_ret_excess
  alpha_CAPM <- coef(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test))[1]
  alpha_CAPM_t <- coeftest(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test), vcov = NeweyWest(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test)))[1, 3]
  alpha_CAPM_p <- coeftest(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test), vcov = NeweyWest(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test)))[1, 4]
  alpha_3F <- coef(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test))[1]
  alpha_3F_t <- coeftest(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test), vcov = NeweyWest(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test)))[1, 3]
  alpha_3F_p <- coeftest(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test), vcov = NeweyWest(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test)))[1, 4]
  
  table_data1 <- data.frame(
    "Statistic" = c("Average return", "SD return", "Sharpe ratio", "Max. weight",
                    "Min. weight", "Avg. sum of negative weights", "Avg. fraction of negative weights"),
    "Value" = c(avg_return, sd_return, sharpe_ratio, max_weight, min_weight,
                avg_neg_weights, frac_neg_weights)
  )
  
  table_data2 <- data.frame(
    "Statistic" = c("CAPM alpha", "CAPM alpha t-stat", "CAPM alpha p-value","FF3 alpha", "FF3 alpha t-stat", "FF3 alpha p-value"),
    "Value" = c(alpha_CAPM, alpha_CAPM_t, alpha_CAPM_p,alpha_3F, alpha_3F_t, alpha_3F_p)
  )
  
  
  # Display the tables side by side
  knitr::kable(
    list(table_data1, table_data2),
    caption = 'Performance Metrics.',
    booktabs = TRUE, valign = 't', digits = 3
  )  %>%
    kable_styling(bootstrap_options = c("striped"))
}