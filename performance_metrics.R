extract_weights <- function(backt_object) {
  return(do.call(rbind, lapply(backt_object, unlist)))
}

pf_weights_longer <- function(backtest_object, stocks_preselected=TRUE){
  dates_testing <- sort(data_predef_stocks$dates)[first_test:end_test]
  if (stocks_preselected) {
    returns <- returns_short
    
  } else {
    returns <- returns_all
  }
  pf_df <-  as.data.frame(extract_weights(backtest_object))  %>%
    rename_all(~ colnames(returns))
  pf_df$Dates <- as.Date(dates_testing, format = "%Y-%m-%d")
  pf_df_long <- pivot_longer(pf_df, cols = -Dates, names_to = "Stock", values_to = "Weights")
  return(pf_df_long)
  
}
#' Generate Performance Metrics
#'
#' This function calculates various performance metrics for a portfolio based on given weights and returns data.
#'
#' @param weights A matrix or data frame containing the weights assigned to each asset in the portfolio.
#' @param stocks_preselected Boolean indicating whether the stocks are preselected (default: TRUE). If set to FALSE, missing returns will be replaced with zero.
#' @param returns A matrix or data frame containing the historical returns of the assets.
#'
#' @return A formatted table displaying the performance metrics, including average return, standard deviation of return, Sharpe ratio, maximum and minimum weights, average sum of negative weights, average fraction of negative weights, CAPM alpha, CAPM alpha t-statistic, CAPM alpha p-value, Fama-French three-factor alpha, Fama-French three-factor alpha t-statistic, and Fama-French three-factor alpha p-value.
#'
#' @importFrom knitr kable kable_styling
#' @importFrom stats coef lm
#' @importFrom sandwich NeweyWest
#' @importFrom lmtest coeftest
#' @importFrom dplyr filter mutate


generate_performance_metrics <- function(backtest_object, stocks_preselected = TRUE, returns) {
  
  dates_testing <- sort(data_predef_stocks$dates)[first_test:end_test]
  FF_factors_test <- FF_factors %>% filter(date %in% dates_testing)
  test_smpl_returns <- as.matrix(returns[first_test:end_test, ])
  
  if (stocks_preselected==FALSE) {
    test_smpl_returns <- replace(test_smpl_returns, is.na(test_smpl_returns), 0)
    
  } 
  weights <- extract_weights(backtest_object)
  pf_returns <- rowSums(weights*test_smpl_returns)
  pf_ret_excess <- pf_returns
  avg_return <- mean(pf_returns, na.rm = TRUE)
  sd_return <- sd(pf_returns, na.rm = TRUE)
  sharpe_ratio <- mean(pf_ret_excess) / sd_return
  max_weight <- max(weights)
  min_weight <- min(weights)
  avg_neg_weights <- mean(weights[weights < 0])
  frac_neg_weights <- mean(apply(weights, 1, function(row) sum(row[row < 0])))
  frac_pos_weights <- mean(apply(weights, 1, function(row) sum(row[row > 0])))
  FF_factors_test$pf_ret_excess <- pf_ret_excess
  alpha_CAPM <- coef(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test))[1]
  alpha_CAPM_t <- coeftest(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test), vcov = NeweyWest(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test)))[1, 3]
  alpha_CAPM_p <- coeftest(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test), vcov = NeweyWest(lm(pf_ret_excess ~ Mkt.RF,FF_factors_test)))[1, 4]
  alpha_3F <- coef(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test))[1]
  alpha_3F_t <- coeftest(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test), vcov = NeweyWest(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test)))[1, 3]
  alpha_3F_p <- coeftest(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test), vcov = NeweyWest(lm(pf_ret_excess ~ Mkt.RF + SMB + HML,FF_factors_test)))[1, 4]
  
  table_data1 <- data.frame(
    "Statistic" = c("Average return", "SD return", "Sharpe ratio", "Max. weight",
                    "Min. weight", "Avg. sum of negative weights", "Avg. fraction of negative weights","Avg. fraction of positive weights" ),
    "Value" = c(avg_return, sd_return, sharpe_ratio, max_weight, min_weight,
                avg_neg_weights, frac_neg_weights,frac_pos_weights)
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