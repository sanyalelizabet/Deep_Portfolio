w_full_constraint <- function(x) {
  backend <-  backend()
  constant_two <- (k_sum(x, axis = 2, keepdims = TRUE)+backend$epsilon())
  output <- tf$divide(x, constant_two)
  
  return(output)
}


w_full_constraint_leverage <- function(x) {
  backend <-  backend()
  constant <- (k_sum(x, axis = 2, keepdims = TRUE)+backend$epsilon())
  output_norm <- tf$divide(x, constant)
  output_clipped <- tf$clip_by_value(output_norm, -0.5, 0.55)
  constant_two <- (k_sum(output_clipped, axis = 2, keepdims = TRUE)+backend$epsilon())
  output <- tf$divide(output_clipped, constant_two)
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


print_layer_lin_output <- function(x) {
  k_print_tensor(x, "That output linear")
  
  return(x)
}
print_layer_non_lin_output <- function(x) {
  k_print_tensor(x, "That output non linear")
  
  return(x)
}

print_layer_cont_output <- function(x) {
  k_print_tensor(x, "That output contac")
  
  return(x)
}