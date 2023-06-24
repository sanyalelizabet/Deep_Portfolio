w_full_constraint <- function(x) {
  backend <-  backend()
  constant_two <- (k_sum(x, axis = 2L, keepdims = TRUE)+backend$epsilon())
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
  portfolio_return <- k_sum(weighted_returns, axis = 2L)
  
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
  portfolio_return <- k_sum(weighted_returns, axis = 2L)
  
  mean_return <- backend$mean(portfolio_return)
  std_return <- backend$std(portfolio_return)+ backend$constant(0.001)
  sharpe_ratio <- (mean_return+backend$constant(0.001)) / (std_return)
  
  desired_shape <- shape(12, 1)
  repeated_tensor <- tf$fill(dims = desired_shape, value = -sharpe_ratio)
  
  return(-repeated_tensor)
}

#' Scale the rows of a matrix
#'
#' This code defines a function called row_scale that scales the rows of a matrix using a specified backend library, likely TensorFlow. 
row_scale <- function(x) {
  row_mean <- tf$reduce_mean(x, axis = 1L, keepdims = TRUE)
  row_std <- tf$math$reduce_std(x, axis = 1L, keepdims = TRUE)
  scaled_tensor <- (x - row_mean) / row_std
  return(scaled_tensor)
}

#' Sharpe Ratio Loss Function with Transaction Cost
#'
#' Calculates the Sharpe Ratio loss function with transaction cost given the predicted weights and returns.
#'
#' @param y_ret Tensor of actual returns.
#' @param y_pred Tensor of predicted weights.
#'
#' @return Tensor containing the calculated loss value.
#'
#' @details This loss function calculates the Sharpe Ratio with transaction cost as the loss value. It considers the predicted weights and actual returns to compute the portfolio return, transaction cost, mean return, and standard deviation of returns. The Sharpe Ratio is then calculated by subtracting the transaction cost from the mean return and dividing it by the standard deviation.
#' The loss value is returned as a tensor with the same shape as the input tensors.
#'

sharpe_ratio_tc_loss <- function(y_ret, y_pred) {
  backend <-  backend()
  y_ret <- tf$cast(y_ret, dtype=tf$float32)  
  y_pred <- tf$cast(y_pred, dtype=tf$float32) 
  weighted_returns <-  y_ret*y_pred
  
  portfolio_return <- k_sum(weighted_returns, axis = 2)
  
  y_pred_net <- y_ret + 1
  prior_weights <- y_pred*y_pred_net
  
  weights_to_rebalance <-   tf$strided_slice( y_pred,
                                              begin = c(1L, 0L),
                                              end = c(tf$shape(y_pred)[1], tf$shape(y_pred)[2]),
                                              strides = c(1L, 1L) # Strides for each axis
  ) 
  prior_weights_stride <- tf$strided_slice(
    prior_weights,
    begin = c(0L, 0L),
    end = c(tf$shape(y_pred)[1] - 1L, tf$shape(y_pred)[2]),
    strides = c(1L, 1L)
  )
  turn <-k_sum(tf$abs(weights_to_rebalance - prior_weights_stride),axis = 2)/k_sum(prior_weights_stride, axis = 2)
  
  transaction_cost <- 0.005*k_mean(turn)
  
  mean_return <- backend$mean(portfolio_return)
  std_return <- backend$std(portfolio_return)+ backend$constant(0.001)
  
  sharpe_ratio <- (mean_return-transaction_cost) / (std_return)
  desired_shape <- shape(batch_size, 1)
  
  return(-tf$fill(dims = desired_shape, value = -sharpe_ratio))
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






