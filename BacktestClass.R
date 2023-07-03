#' Backtest Class
#'
#' This class represents a backtest object.
#'
#' @field data_wide The input data in wide format.
#' @field returns The matrix or data frame of returns.
#' @field train_window The length of initial training.
#' @field val_window The length of validation window.
#' @field model The model used for prediction.
#' @field window The window type for the training data fixed or extanding.
#' @field stocks_preselected Boolean indicating whether the stocks are preselected.
#' @field transaction_cost Boolean indicating whether transaction costs are considered.
#' @field activation_nl_i The activation function for the nonlinear layer I.
#' @field activation_conc The activation function for the concatination layer.
#' @field learning_rate The learning rate for the optimization algorithm.
#' @field activation_nl_ii The activation function for the nonlinear layer II.
#' @field units_choice The number of units for the fully connected layer.
#' @field epochs The number of training epochs.
#' @field rate The regularization parameter for the optimization algorithm.
#' @field batch_size The batch size for training.
Backtest <- R6Class("Backtest", lock_objects = FALSE,
                    public = list(# Fields
                      data_wide = NULL,
                      returns = NULL,
                      train_window = NULL,
                      val_window = NULL,
                      model = NULL,
                      window = NULL,
                      stocks_preselected = NULL,
                      transaction_cost = NULL,
                      activation_nl_i = NULL,
                      activation_conc = NULL,
                      learning_rate = NULL,
                      activation_nl_ii = NULL,
                      units_choice = NULL,
                      epochs = NULL,
                      rate = NULL,
                      batch_size = NULL,
                      
                      initialize = function(data_wide=NULL, returns=NULL,train_window=60,val_window=60, model = "simple", window = "expand",
                                            stocks_preselected = FALSE, transaction_cost = FALSE,
                                            activation_nl_i = "sigmoid", activation_conc = "swish",
                                            learning_rate = 0.01, activation_nl_ii = "tanh",
                                            units_choice = 10, epochs = 50, rate = 0.1, batch_size = 12) {
                        
                        
                        # Store the inputs as attributes
                        self$data_wide <- data_wide
                        self$returns <- returns
                        self$model <- model
                        self$window <- window
                        self$stocks_preselected <- stocks_preselected
                        self$transaction_cost <- transaction_cost
                        self$activation_nl_i <- activation_nl_i
                        self$activation_conc <- activation_conc
                        self$learning_rate <- learning_rate
                        self$activation_nl_ii <- activation_nl_ii
                        self$units_choice <- units_choice
                        self$epochs <- epochs
                        self$rate <- rate
                        self$batch_size <- batch_size
                        
                        self$val_window <- val_window
                        self$training_window <- train_window
                        self$first_test <- val_window + training_window
                        
                        self$testing_period_len <-  nrow(data_wide)-training_window-val_window
                        self$testing_periods_seq <- seq(training_window, training_window+testing_period_len)
                        self$end_test <- first_test+testing_period_len
                        
                        
                      },
                      
                      print = function() {
                        cat("Backtest Parameters:\n")
                        cat("Model: ", self$model, "\n")
                        cat("Window Type: ", self$window, "\n")
                        cat("Stocks Preselected: ", self$stocks_preselected, "\n")
                        cat("Transaction Cost: ", self$transaction_cost, "\n")
                      }
                    ,
                      
                   apply_backtest_to_periods = function() {
                        results <- purrr::map(self$testing_periods_seq, backtest, self$data_wide, self$returns, 
                                       model = self$model, window = self$window, stocks_preselected = self$stocks_preselected, 
                                       transaction_cost = self$transaction_cost)
                        return(results)
                                         },
                      backtest = function(t, data_wide, returns, model="simple",
                                           window="fixed", stocks_preselected=TRUE, 
                                           transaction_cost=FALSE) {
                        # Parameter Validation
                        stopifnot(is.numeric(t),
                                  t >= 1 && t <= nrow(data_wide),
                                  is.data.frame(data_wide),
                                  is.data.frame(returns),
                                  window %in% c("fixed", "expand"),
                                  model %in% c("simple", "lstm", "complex"),
                                  is.logical(stocks_preselected),
                                  is.logical(transaction_cost)
                        )
                        simple <- model == "simple"
                        n_stock_selected <- ncol(returns)
                        print(t)
                        
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
                                                                      patience = 5,
                                                                      verbose = 1,
                                                                      mode = 'auto'
                        )
                        
                        if (stocks_preselected) {
                          if (anyNA(returns)) {
                            stop("NA values found in returns. Please ensure returns do not have missing values.")
                          }
                          loss_fun <- sharpe_ratio_loss
                          
                          if (transaction_cost){
                            loss_fun <- sharpe_ratio_tc_loss
                          }
                          
                          
                        } else {
                          loss_fun <- sharpe_ratio_loss_na_check
                          
                          if (transaction_cost){
                            loss_fun <- sharpe_ratio_tc_loss_na_check
                          }
                        }
                        
                        if (simple){
                          
                          model <- build_simple_model(n_stock_selected, rate, units_choice, 
                                                      activation_nl_i, learning_rate, activation_nl_ii, 
                                                      data_wide,loss_fun )
                          
                          model %>% fit( x = window_data, 
                                         y = window_returns, 
                                         epochs = epochs, 
                                         batch_size = batch_size, 
                                         verbose = 2,  
                                         validation_data = list(val_window_data, val_returns),
                                         shuffle=FALSE,
                                         callbacks = list(early_stopping,lr_callback)
                          )
                          
                          prediction_w_t <- predict(model, data_predict)
                          
                        } else {
                          
                          
                          if (model == "complex") {
                            
                            model <- build_model(n_stock_selected, rate, units_choice, 
                                                 activation_nl_i,activation_conc,
                                                 learning_rate,activation_nl_ii, 
                                                 data_wide,loss_fun)
                            
                            model %>% fit( x = list(window_data, window_data), 
                                           y = window_returns, 
                                           epochs = epochs, 
                                           batch_size = batch_size, 
                                           verbose = 2,  
                                           validation_data = list(list(val_window_data, val_window_data), val_returns),
                                           shuffle=FALSE,
                                           callbacks = list(early_stopping, lr_callback)
                            )
                            
                            prediction_w_t <- predict(model, list(data_predict,data_predict))
                          } else {  #lstm
                            data_lstm_train <- timeseries_dataset_from_array(window_data, window_returns, 12)
                            lstm_features_train<- data_lstm_train$features
                            lstm_features_target <- data_lstm_train$target 
                            
                            data_lstm_val <- timeseries_dataset_from_array(val_window_data, val_returns, 12)
                            lstm_features_val<- data_lstm_val$features
                            lstm_target_val <- data_lstm_val$target
                            
                            data_predict <- as.matrix(data_wide[(t+val_window-11):(t+val_window),])
                            return_observed <- as.matrix(returns[(t+val_window-11):(t+val_window),])
                            data_lstm_predict <- timeseries_dataset_from_array(data_predict, return_observed, 12)
                            lstm_feat_pred <- data_lstm_predict$features
                            
                            model <- build_lstm_model(n_stock_selected, rate, units_choice,
                                                      learning_rate, lstm_features_train, loss = loss_fun,timesteps=12)
                            
                            model %>% fit(
                              lstm_features_train ,lstm_features_target ,
                              epochs = epochs,
                              batch_size = 1,
                              validation_data = list(lstm_features_val, lstm_target_val),
                              callbacks = list(early_stopping, lr_callback)
                            )
                            
                            prediction_w_t <- predict(model, lstm_feat_pred)
                            
                          }
                        }
                        
                        if (stocks_preselected) {
                          # If stocks are preselected, the predicted weights are returned as is.
                          return(prediction_w_t)
                          
                        } else {
                          return_data_predict <- as.matrix(returns[(t+val_window),])
                          
                          stocks_available  <- is.na(return_data_predict)
                          # If stocks are not preselected, any stocks with unavailable returns are assigned weights of 0.
                          # The predicted weights are then normalized again so that they sum to 1.
                          prediction_w_t[stocks_available] <- 0
                          
                          # Separate negative and positive values
                          negative_values <- prediction_w_t[prediction_w_t < 0]
                          positive_values <- prediction_w_t[prediction_w_t > 0]
                          
                          # Normalize negative values
                          if (length(negative_values) > 0) {
                            negative_values <- -0.2*negative_values/sum(negative_values)
                          }
                          
                          # Normalize positive values
                          if (length(positive_values) > 0) {
                            positive_values <- 1.2*positive_values / sum(positive_values)
                          }
                          
                          # Combine normalized values
                          normalized_prediction_w_t <- prediction_w_t
                          normalized_prediction_w_t[prediction_w_t < 0] <- negative_values
                          normalized_prediction_w_t[prediction_w_t > 0] <- positive_values
                          
                          return(normalized_prediction_w_t)
                        }
                        
                        
                      }
                      
                      
                     
                    )
)

                        