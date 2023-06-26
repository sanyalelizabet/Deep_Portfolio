# backtest: Perform backtesting of a trading strategy
#
# Args:
#   t (numeric): The current time index.
#   data_wide (matrix): Matrix of historical data.
#   returns (matrix): Matrix of historical returns.
#   simple (logical): Indicates whether to use a simple model (default is FALSE).
#   window (character): Specifies the window type for training data (default is "fixed").
#   stocks_preselected (logical): Indicates whether stocks are preselected (default is TRUE).
#   transaction_cost (logical): Indicates whether to consider transaction costs (default is TRUE).
#
# Returns:
#   numeric: Predicted weights for the next time period based on the backtest.
#
# Details:
#   The `backtest` function performs backtesting of a trading strategy using a specified model. It takes as input the current time index, historical data, and returns. The function builds a model based on the specified parameters and performs training using the training data. The trained model is then used to predict weights for the next time period. The function returns the predicted weights.

#' Perform backtesting of a trading strategy
#'
#' Perform backtesting of a trading strategy using a specified model.
#'
#' @param t The current time index.
#' @param data_wide Matrix of historical data.
#' @param returns Matrix of  historical returns.
#' @param simple Indicates whether to use a simple model. Default is FALSE.
#' @param window Specifies the window type for training data. Default is "fixed".
#' @param stocks_preselected Indicates whether stocks are preselected. Default is TRUE.
#' @param transaction_cost Indicates whether to consider transaction costs. Default is FALSE.
#'
#' @return Predicted weights for the next time period based on the backtest.
#'
#' @details The `backtest` function performs backtesting of a trading strategy using a specified model. It takes as input the current time index, historical data, and returns. The function builds a model based on the specified parameters and performs training using the training data. The trained model is then used to predict weights for the next time period. The function returns the predicted weights.
#'
backtest <- function(t, data_wide, returns, simple=FALSE,
                     window="fixed", stocks_preselected=TRUE, 
                     transaction_cost=FALSE) {
  # Parameter Validation
  stopifnot(is.numeric(t),
            t >= 1 && t <= nrow(data_wide),
            is.data.frame(data_wide),
            is.data.frame(returns),
            window %in% c("fixed", "expand"),
            is.logical(simple),
            is.logical(stocks_preselected),
            is.logical(transaction_cost)
  )
  
  n_stock_selected <- ncol(returns)
  
  if (window=="fixed"){
      
      window_data <- as.matrix(data_wide[(t-training_window+1):t,])
      window_returns <- as.matrix(returns[(t-training_window+1):t,])
      val_window_data <- as.matrix(data_wide[t:(t+val_window-1),])
      val_returns <- as.matrix(returns[t:(t+val_window-1),])
      data_predict <- as.matrix(data_wide[(t+val_window),])
    
  } else {
    
    first <- (training_window+t)%%6+1
    window_data <- as.matrix(data_wide[first:(t+training_window),])
    window_returns <- as.matrix(returns[first:(t+training_window),])
    val_window_data <- as.matrix(data_wide[t:(t+val_window-1),])
    val_returns <- as.matrix(returns[t:(t+val_window-1),])
    data_predict <- as.matrix(data_wide[(t+val_window),])
    
    
  }
  
  
  early_stopping <- callback_early_stopping(monitor = c('val_loss'),
                                            patience = 50,
                                            restore_best_weights = TRUE
                                            )
  
  lr_callback <- callback_reduce_lr_on_plateau( monitor = 'val_loss',
                                                factor = 0.01, 
                                                patience = 10,
                                                verbose = 1,
                                              mode = 'auto'
                                              )
  
  if (stocks_preselected==TRUE) {
    
    if (anyNA(returns)) {
      stop("NA values found in returns. Please ensure returns are complete.")
    }
    loss_fun <- sharpe_ratio_loss
    
    if (transaction_cost==TRUE){
      loss_fun <- sharpe_ratio_tc_loss
    }
    
    
  } else {
    loss_fun <- sharpe_ratio_loss_na_check
    
    if (transaction_cost==TRUE){
      loss_fun <- sharpe_ratio_tc_loss_na_check
    }
  }
   
  if (simple==TRUE){
   
    model <- build_simple_model(n_stock_selected, rate, units_choice, 
                                activation_nl_i, learning_rate, activation_nl_ii, 
                                data_wide,loss_fun )
    
    model %>% fit( x = window_data, 
                   y = window_returns, 
                   epochs = epochs, 
                   batch_size = 12, 
                   verbose = 2,  
                   validation_data = list(val_window_data, val_returns),
                   shuffle=FALSE,
                   callbacks = list(early_stopping, lr_callback)
    )
    
    prediction_w_t <- predict(model, data_predict)
    
  } else {
    
    model <- build_model(n_stock_selected, rate, units_choice, 
                         activation_nl_i,activation_conc,
                         learning_rate,activation_nl_ii, 
                         data_wide,loss_fun)
    
    model %>% fit( x = list(window_data, window_data), 
                   y = window_returns, 
                   epochs = epochs, 
                   batch_size = 12, 
                   verbose = 2,  
                   validation_data = list(list(val_window_data, val_window_data), val_returns),
                   shuffle=FALSE,
                   callbacks = list(early_stopping, lr_callback)
    )
    
    prediction_w_t <- predict(model, list(data_predict,data_predict))
  }
 
  if (stocks_preselected==TRUE) {
    # If stocks are preselected, the predicted weights are returned as is.
    return(prediction_w_t)
    
  } else {
    return_data_predict <- as.matrix(returns[(t+val_window),])
    stocks_available  <- is.na(return_data_predict)
    # If stocks are not preselected, any stocks with unavailable returns are assigned weights of 0.
    # The predicted weights are then normalized and clipped between -0.1 and 0.1.
    prediction_w_t[stocks_available] <- 0
    prediction_w_t <- prediction_w_t/sum(prediction_w_t)
    prediction_w_t[prediction_w_t > 0.1] <- 0.1
    prediction_w_t[prediction_w_t < -0.1] <- -0.1
    prediction_w_t <- prediction_w_t/sum(prediction_w_t)
    return(prediction_w_t)
  }
  
  
}


