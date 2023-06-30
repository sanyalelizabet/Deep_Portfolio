#' Extract Portfolio Weights
#'
#' This function extracts the portfolio weights from a backtest object.
#'
#' @param backt_object The backtest object containing the portfolio weights.
#'
#' @return A matrix or data frame of the extracted portfolio weights.

extract_weights <- function(backt_object) {
  return(do.call(rbind, lapply(backt_object, unlist)))
}
#' Portfolio Weights to Long Format
#'
#' This function converts the portfolio weights from a backtest object into a long format.
#'
#' @param backtest_object The backtest object containing the portfolio weights.
#' @param stocks_preselected Boolean indicating whether the stocks are preselected (default: TRUE).
#' @param data_predef_stocks The data frame containing predefined stocks and dates.
#' @param first_test The index of the first testing date.
#' @param end_test The index of the last testing date.
#' @param returns_short The matrix or data frame of short-term returns.
#' @param returns_all The matrix or data frame of all returns.
#'
#' @return A long format data frame with columns for Dates, Stock, and Weights.
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

#' Calculate Turnover
#'
#' This function calculates the turnover of a portfolio based on the provided weights and returns.
#'
#' @param weights A matrix or data frame containing the weights assigned to each asset in the portfolio.
#' @param returns A matrix or data frame containing the historical returns of the assets.
#' @param stocks_preselected Boolean indicating whether the stocks are preselected (default: TRUE). If set to FALSE, missing returns will be replaced with zero.
#'
#' @return The average turnover of the portfolio.
calculate_turnover <- function(weights, return) {
  nan_ret_to_zero <- ifelse(!is.na(return), return, 0)
  weighted_returns <- nan_ret_to_zero * weights
  portfolio_return <- rowSums(weighted_returns)
  
  net_return <- nan_ret_to_zero + 1
  prior_weights <- weights * net_return
  
  weights_to_rebalance <- weights[1:(nrow(weights)-1), ]
  prior_weights_stride <- prior_weights[1:(nrow(weights)-1), ]
  turn <- rowSums(abs(weights_to_rebalance - prior_weights_stride)) / rowSums(prior_weights_stride)
  return(mean(turn)) 
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
  turn <- calculate_turnover(weights, test_smpl_returns)
  pf_returns <- rowSums(weights*test_smpl_returns)
  pf_ret_excess <- pf_returns
  avg_return <- mean(pf_returns, na.rm = TRUE)
  sd_return <- sd(pf_returns, na.rm = TRUE)
  sharpe_ratio <- mean(pf_ret_excess) / sd_return
  sharpe_ratio_tc_adj <-  (avg_return -0.005*calculate_turnover(weights, test_smpl_returns)) / sd_return
  max_weight <- max(weights)
  min_weight <- min(weights)
  avg_neg_weights <- mean(weights[weights < 0])
  avg_pos_weights <- mean(weights[weights > 0])
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
    "Statistic" = c("Average return", "SD return", "Sharpe ratio","Sharpe ratio TC Adj.", 
                    "Average Turnover", "Max. weight",
                    "Min. weight", "Avg. negative weights","Avg. positive weights",  
                    "Avg. fraction of negative weights","Avg. fraction of positive weights" ),
    "Value" = c(avg_return, sd_return, sharpe_ratio,sharpe_ratio_tc_adj, turn, max_weight, min_weight,
                avg_neg_weights, avg_pos_weights, frac_neg_weights,frac_pos_weights)
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


#' Plot Cumulative Performance
#'
#' This function plots the cumulative performance of a portfolio compared to an equally weighted portfolio.
#'
#' @param backtest_object A backtest object containing weights for the portfolio.
#' @param returns A matrix or data frame containing the returns of individual assets.
#' @return A ggplot object representing the cumulative performance plot.
#'
plot_cumulative <- function(backtest_object, returns) {
  weights <- extract_weights(backtest_object)
  test_smpl_returns <- as.matrix(returns[first_test:end_test, ])
  portfolio_returns <- rowSums(weights*test_smpl_returns)
  # Calculate cumulative returns for each asset
  cumulative_returns <- cumprod(1 + portfolio_returns)
  
  # Calculate equally weighted portfolio returns
  equally_weights <- 1 / ncol(test_smpl_returns)
  ew_returns <- rowSums(equally_weights*test_smpl_returns)
  cumulative_equally_weighted <- cumprod(1 + ew_returns)
  dates_testing <- sort(data_predef_stocks$dates)[first_test:end_test]
  data <- data.frame(
    Date = dates_testing,
    PortfolioReturn = cumulative_returns,
    EquallyWeightedReturn = cumulative_equally_weighted
  )
  
  # Convert the data frame to long format
  data_long <- pivot_longer(
    data,
    cols = c(PortfolioReturn, EquallyWeightedReturn),
    names_to = "Type",
    values_to = "CumulativeReturn"
  )
  
  
  # Create a data frame with portfolio returns
  data_ret <- data.frame(Date = dates_testing,
    PortfolioReturn = portfolio_returns,
    EquallyWeightedReturn = ew_returns
  )
  
  # Convert data to long format
  data_ret_long <- tidyr::pivot_longer(data_ret, cols = c(PortfolioReturn, EquallyWeightedReturn), names_to = "Type", values_to = "Returns")
  # Plot the cumulative performance relative to equally weighted portfolio
  plot1 <- ggplot(data_long, aes(x = Date, y = CumulativeReturn, color = Type)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("#4C72B0", "#55A868"), labels = c("Equally Weighted Return", "Portfolio Return")) +
    labs(x = "Date", y = "Cumulative Performance", title = "Cumulative Performance") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
  # Create a histogram of portfolio and equally weighted returns
  plot2 <- ggplot(data_ret_long, aes(x = Returns, fill = Type)) +
    geom_histogram(binwidth = 0.02, alpha = 0.7) +
    scale_fill_manual(values = c("#4C72B0", "#55A868"), labels = c("Portfolio Return", "Equally Weighted Return")) +
    labs(x = "Returns", y = "Frequency", title = "Distribution of Returns") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
 
  plot <- grid.arrange(plot1, plot2 + guides(fill = FALSE), ncol = 2)
  return(plot)
}