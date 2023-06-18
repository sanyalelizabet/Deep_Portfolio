w_full_constraint <- function(x) {
  backend <-  backend()
  constant_two <- (k_sum(x, axis = 2, keepdims = TRUE)+backend$epsilon())
  output <- tf$divide(x, constant_two)
  
  return(output)
}

sharpe_ratio_loss <- function(y_ret, y_pred) {
  backend <-  backend()
  weighted_returns <-  y_ret*y_pred
  portfolio_return <- k_sum(weighted_returns, axis = 2)
  #k_print_tensor(portfolio_return, "That returns of the batch portfolios")
  mean_return <- backend$mean(portfolio_return)
  std_return <- backend$std(portfolio_return)
  sharpe_ratio <- (mean_return+backend$constant(0.001)) / (std_return + backend$constant(0.001))
  
  return(-sharpe_ratio)
}