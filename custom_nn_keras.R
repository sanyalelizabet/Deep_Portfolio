#' Full Constraint Function
#'
#' Applies full constraint to the input weights tensor.
#'
#' @param x Tensor of weights.
#'
#' @return Tensor containing the output after applying full constraint.
#'
#' @details This function applies full constraint to the input tensor of weights. It calculates the sum of weights along axis 2 and adds a small epsilon value to avoid division by zero. The output is then normalized by dividing each element by the sum.
#' The resulting tensor contains the output after applying full constraint to the input tensor.
#'

w_full_constraint <- function(x) {
  backend <-  backend()
  #x <- x+1/tf$cast(tf$shape(x)[2], dtype=tf$float32)   
  constant_two <- (k_sum(x, axis = 2L, keepdims = TRUE)+backend$epsilon())
  output <- tf$divide(x, constant_two)
  
  return(output)
}


#' Full Constraint Leverage Function
#'
#' Applies full constraint leverage to the input weights tensor.
#'
#' @param x Tensor of weights.
#'
#' @return Tensor containing the output after applying full constraint leverage.
#'
#' @details This function applies full constraint leverage to the input tensor of weights. It calculates the sum of weights along axis 2 and adds a small epsilon value to avoid division by zero. The output is then normalized by dividing each element by the sum. Next, the normalized output is clipped to the range [-0.5, 0.5] using tf$clip_by_value function. Finally, the clipped output is again normalized by dividing each element by the sum after clipping.
#' The resulting tensor contains the output after applying full constraint leverage to the input tensor.
#'
w_full_constraint_leverage <- function(x) {
  
  negative_w <- tf$math$less_equal(x, tf$constant(0, dtype = tf$float32))
  
  masked_neg_w <- tf$where(negative_w, x, tf$zeros_like(x))
  masked_pos_w <- tf$where(negative_w, tf$zeros_like(x), x)
 
  pos_w_scaled <-  1.2*tf$tanh(masked_pos_w)
  neg_w_scaled <- -0.2*tf$tanh(masked_neg_w)
  pos_w_scaled <- tf$divide(pos_w_scaled, k_sum(tf$tanh(masked_pos_w), axis = 2L, keepdims = TRUE))
  neg_w_scaled <- tf$divide(neg_w_scaled, k_sum(tf$tanh(masked_neg_w), axis = 2L, keepdims = TRUE))
  constrained_w <- tf$where(negative_w, neg_w_scaled, pos_w_scaled)
  
  
  return(constrained_w)
}
 
#' Sharpe Ratio Loss Function
#'
#' Calculates the Sharpe Ratio loss function given the predicted weights and observed returns.
#'
#' @param y_ret Tensor of actual returns.
#' @param y_pred Tensor of predicted weights.
#'
#' @return Tensor containing the calculated loss value.
#'
#' @details This loss function calculates the Sharpe Ratio as the loss value. It considers the predicted weights and actual returns to compute the portfolio return, mean return, and standard deviation of returns. The Sharpe Ratio is then calculated by dividing the mean return by the standard deviation. The loss value is returned as a tensor with the same shape as the input tensors.
#'
sharpe_ratio_loss <- function(y_ret, y_pred) {
  backend <-  backend()
 
  weighted_returns <-  y_ret*y_pred
  
  portfolio_return <- k_sum(weighted_returns, axis = 2L)
  
  mean_return <- backend$mean(portfolio_return)
  
  std_return <- backend$std(portfolio_return)+ backend$constant(0.001)
  sharpe_ratio <- (mean_return+backend$constant(0.001)) / (std_return)
  
  #desired_shape <- shape(batch_size, 1)
  #repeated_tensor <- tf$fill(dims = desired_shape, value = -sharpe_ratio)
  
  return(-sharpe_ratio)
}

#' Sharpe Ratio Loss Function with NaN Handling
#'
#' Calculates the Sharpe Ratio loss function given the predicted weights and returns, with NaN handling.
#'
#' @param y_ret Tensor of actual returns.
#' @param y_pred Tensor of predicted weights.
#'
#' @return Tensor containing the calculated loss value.
#'
#' @details This loss function calculates the Sharpe Ratio as the loss value. It considers the predicted weights and actual returns to compute the portfolio return, mean return, and standard deviation of returns. NaN values in the actual returns are handled by setting them to zero. The Sharpe Ratio is then calculated by dividing the mean return by the standard deviation. The loss value is returned as a tensor with the same shape as the input tensors.
#'
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
  
  #desired_shape <- shape(12, 1)
  #repeated_tensor <- tf$fill(dims = desired_shape, value = -sharpe_ratio)
  
  return(-sharpe_ratio)
}

#' Scale the rows of a matrix
#'
#' This code defines a function called row_scale that scales the rows of a matrix using a specified backend library, likely TensorFlow. 
row_scale <- function(x) {
 
  row_mean <- tf$reduce_mean(x, 
                             axis = 1L, 
                             keepdims = TRUE)
  
 
  row_std <- tf$math$reduce_std(x, 
                                axis = 1L,
                                keepdims = TRUE)
 
  scaled_tensor <- tf$where(row_std > 0, (x - row_mean) / row_std, (x - row_mean))
  #scaled_tensor <- (x - row_mean) / row_std
  return(scaled_tensor)
}


row_l2 <- function(x) {
  x <- k_l2_normalize(x, axis = 1)
  return(x)
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
  y_ret <- tf$cast(y_ret, 
                   dtype=tf$float32)  
  y_pred <- tf$cast(y_pred, 
                    dtype=tf$float32) 
  weighted_returns <-  y_ret*y_pred
  
  portfolio_return <- k_sum(weighted_returns, axis = 2)
  
  y_pred_net <- y_ret + 1
  prior_weights <- y_pred*y_pred_net
  
  weights_to_rebalance <-   tf$strided_slice( y_pred,
                                              begin = c(1L, 0L),
                                              end = c(tf$shape(y_pred)[1], 
                                                      tf$shape(y_pred)[2]),
                                              strides = c(1L, 1L) # Strides for each axis
  ) 
  prior_weights_stride <- tf$strided_slice(prior_weights,
                                           begin = c(0L, 0L),
                                           end = c(tf$shape(y_pred)[1] - 1L, 
                                                   tf$shape(y_pred)[2]),
                                           strides = c(1L, 1L)
                                           )
  turn <-k_sum(tf$abs(weights_to_rebalance - prior_weights_stride),
               axis = 2)/k_sum(prior_weights_stride, axis = 2)
 
  transaction_cost <- 0.005*k_mean(turn)
  
  mean_return <- backend$mean(portfolio_return)
  std_return <- backend$std(portfolio_return)+backend$constant(0.001)
  
  sharpe_ratio <- (mean_return-transaction_cost) / (std_return)
  #desired_shape <- shape(batch_size, 1)
  #-tf$fill(dims = desired_shape, value = -sharpe_ratio)
  return(-sharpe_ratio)
}

#' Sharpe Ratio Loss Function with Transaction Cost and Handling the Missing Stocks
#'
#' Calculates the Sharpe Ratio loss function with transaction cost given the predicted weights and returns.
#'
#' @param y_ret Tensor of actual returns. NaN values are handled.
#' @param y_pred Tensor of predicted weights.
#'
#' @return Tensor containing the calculated loss value.
#'
#' @details This loss function calculates the Sharpe Ratio with transaction cost as the loss value. It considers the predicted weights and actual returns to compute the portfolio return, transaction cost, mean return, and standard deviation of returns. NaN values in the actual returns are handled by setting them to zero. The Sharpe Ratio is then calculated by subtracting the transaction cost from the mean return and dividing it by the standard deviation.
#' The loss value is returned as a tensor with the same shape as the input tensors.
#'

sharpe_ratio_tc_loss_na_check  <- function(y_ret, y_pred) {
  y_ret <- tf$cast(y_ret, dtype=tf$float32)  
  y_pred <- tf$cast(y_pred, dtype=tf$float32) 
  backend <-  backend()
  has_nan <- tf$equal(y_ret, y_ret) 
  has_nan <-  k_cast_to_floatx(has_nan)
  
  nan_ret_to_zero <- tf$where(tf$equal(y_ret, y_ret), y_ret, tf$zeros_like(y_ret) )
  nan_w_to_zero <- tf$math$multiply(y_pred, has_nan)
  nan_w_to_zero <- nan_w_to_zero/k_sum(nan_w_to_zero) #Weiths we are working with
  
  weighted_returns <-  nan_ret_to_zero*nan_w_to_zero
  portfolio_return <- k_sum(weighted_returns, axis = 2L)
  
  y_pred_net <- nan_ret_to_zero + 1
  prior_weights <- nan_w_to_zero * y_pred_net
  
  weights_to_rebalance <-   tf$strided_slice( nan_w_to_zero,
                                              begin = c(1L, 0L),
                                              end = c(tf$shape(nan_w_to_zero)[1], 
                                                      tf$shape(nan_w_to_zero)[2]),
                                              strides = c(1L, 1L) # Strides for each axis
  )
  prior_weights_stride <- tf$strided_slice( prior_weights,
                                            begin = c(0L, 0L),
                                            end = c(tf$shape(nan_w_to_zero)[1] - 1L, 
                                                    tf$shape(nan_w_to_zero)[2]),
                                            strides = c(1L, 1L)
  )
  turn <-k_sum(tf$abs(weights_to_rebalance - prior_weights_stride),
               axis = 2)/k_sum(prior_weights_stride, axis = 2)
  
  transaction_cost <- 0.005*k_mean(turn)
  
  mean_return <- backend$mean(portfolio_return)
  std_return <- backend$std(portfolio_return)+ backend$constant(0.001)
  sharpe_ratio <- sharpe_ratio <- (mean_return-transaction_cost) / (std_return)
  
  #desired_shape <- shape(12, 1)
  #-tf$fill(dims = desired_shape, value = -sharpe_ratio)
  return(-sharpe_ratio)
}



print_layer_lin_output <- function(x) {
  k_print_tensor(x, "That output 1 linear")
  
  return(x)
}
print_layer_non_lin_output <- function(x) {
  k_print_tensor(x, "That output non 2 linear")
  
  return(x)
}

print_layer_cont_output <- function(x) {
  print(x)
  k_print_tensor(x, "That output 3")
  
  return(x)
}






