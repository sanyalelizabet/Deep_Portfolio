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
  output_clipped <- tf$clip_by_value(output_norm, -0.5, 0.5)
  constant_two <- (k_sum(output_clipped, axis = 2, keepdims = TRUE)+backend$epsilon())
  output <- tf$divide(output_clipped, constant_two)
  return(output)
}
 


sharpe_ratio_loss <- function(y_ret, y_pred) {
  backend <-  backend()
  weighted_returns <-  y_ret*y_pred
  portfolio_return <- k_sum(weighted_returns, axis = 2)
  
  #print(y_ret)
  #k_print_tensor(portfolio_return, "That returns of the batch portfolios")
  
  mean_return <- backend$mean(portfolio_return)
  std_return <- backend$std(portfolio_return)+ backend$constant(0.001)
 
  sharpe_ratio <- (mean_return+backend$constant(0.001)) / (std_return)
  
  desired_shape <- shape(12, 1)
  repeated_tensor <- tf$fill(dims = desired_shape, value = -sharpe_ratio)
  
  return(-repeated_tensor)
}



sharpe_ratio_loss_na_check <- function(y_ret, y_pred) {
  
  
  backend <-  backend()
  
  has_nan <- tf$equal(y_ret, y_ret) 
  has_nan <-  k_cast_to_floatx(has_nan)
  
  nan_ret_to_zero <- tf$where(tf$equal(y_ret, y_ret),y_ret ,tf$zeros_like(y_ret) )
  
  nan_w_to_zero <- tf$math$multiply(y_pred, has_nan)
  
  nan_w_to_zero <- nan_w_to_zero/k_sum(nan_w_to_zero)
  
  weighted_returns <-  nan_ret_to_zero*nan_w_to_zero
  portfolio_return <- k_sum(weighted_returns, axis = 2)
  
  #print(y_ret)
  #k_print_tensor(portfolio_return, "That returns of the batch portfolios")
  
  mean_return <- backend$mean(portfolio_return)
  std_return <- backend$std(portfolio_return)+ backend$constant(0.001)
  
  sharpe_ratio <- (mean_return+backend$constant(0.001)) / (std_return)
  
  desired_shape <- shape(12, 1)
  repeated_tensor <- tf$fill(dims = desired_shape, value = -sharpe_ratio)
  
  return(-repeated_tensor)
}

#not working.. 
#mean_variance_utility <- function(y_ret, y_pred) {
 # backend <-  backend()
#  weighted_returns <-  y_ret*y_pred
 # portfolio_return <- k_sum(weighted_returns, axis = 2)
  #k_print_tensor(portfolio_return, "That returns of the batch portfolios")
  #mean_return <- backend$mean(portfolio_return)
  #var_return <- backend$std(portfolio_return)^2
  #power_utility <- mean_return-1/2*(var)
  
#  return(-mean_variance_utility)
#}


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






