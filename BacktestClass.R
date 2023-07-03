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
#' @field short_position_size Size of shorted assets Default 0.2 (e.g. 0.2  would give the sum of negative weights -0.2)
#' @field long_position_size Size of long assets (e.g. 1 would give the sum of negative weights 1) Default 1.2
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
                      short_position_size = NULL,
                      long_position_size = NULL,
                      transaction_fee = NULL,
                      
                      initialize = function(data_wide=NULL, returns=NULL,train_window=60,val_window=60, model = "simple", window = "expand",
                                            stocks_preselected = FALSE, transaction_cost = FALSE,
                                            activation_nl_i = "sigmoid", activation_conc = "swish",
                                            learning_rate = 0.01, activation_nl_ii = "tanh",
                                            units_choice = 10, epochs = 50, rate = 0.1, batch_size = 12,
                                            short_position_size = 0.2,long_position_size = 1.2, transaction_fee  = 0.005 ) {
                        
                        
                        
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
                        self$short_position_size <- -short_position_size
                        self$long_position_size <- long_position_size
                        self$transaction_fee <- transaction_fee
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
                    
                    sharpe_ratio_loss = function(y_ret, y_pred) {
                      backend <- backend()
                      weighted_returns <- y_ret * y_pred
                      portfolio_return <- k_sum(weighted_returns, axis = 2L)
                      mean_return <- backend$mean(portfolio_return)
                      std_return <- backend$std(portfolio_return)
                      sharpe_ratio <- (mean_return + backend$constant(0.001)) / std_return
                      return(-sharpe_ratio)
                    },
                    
                    sharpe_ratio_loss_na_check = function(y_ret, y_pred) {
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
                      
                      
                      return(-sharpe_ratio)
                    },
                    sharpe_ratio_tc_loss = function(y_ret, y_pred) {
                      backend <-  backend()
                      y_ret <- tf$reshape(y_ret, shape = c(tf$shape(y_pred)[1], tf$shape(y_pred)[2]))
                      
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
                                                                  strides = c(1L, 1L) 
                      ) 
                      
                      prior_weights_stride <- tf$strided_slice(prior_weights,
                                                               begin = c(0L, 0L),
                                                               end = c(tf$shape(y_pred)[1] - 1L, 
                                                                       tf$shape(y_pred)[2]),
                                                               strides = c(1L, 1L)
                      )
                      
                      turn <-k_sum(tf$abs(weights_to_rebalance - prior_weights_stride),
                                   axis = 2)/k_sum(prior_weights_stride, axis = 2)
                      
                      transaction_cost <- self$transaction_fee*k_mean(turn)
                      
                      mean_return <- backend$mean(portfolio_return)
                      std_return <- backend$std(portfolio_return)+backend$constant(0.001)
                      
                      sharpe_ratio <- (mean_return-transaction_cost) / (std_return)
                      # Check if the tensor is NaN
                      is_nan <- tf$equal(sharpe_ratio, sharpe_ratio) 
                      sharpe_ratio <- tf$where(is_nan, x = sharpe_ratio, y = 0)
                      
                      return(-sharpe_ratio)
                    },
                    sharpe_ratio_tc_loss_na_check  = function(y_ret, y_pred) {
                      y_ret <- tf$reshape(y_ret, shape = c(tf$shape(y_pred)[1], tf$shape(y_pred)[2]))
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
                      
                      transaction_cost <- self$transaction_fee*k_mean(turn)
                      
                      mean_return <- backend$mean(portfolio_return)
                      std_return <- backend$std(portfolio_return)+ backend$constant(0.001)
                      sharpe_ratio <- (mean_return-transaction_cost) / (std_return)
                      
                      
                      return(-sharpe_ratio)
                    },
                    
                    w_full_constraint_leverage = function(x) {
                      
                      negative_w <- tf$math$less_equal(x, tf$constant(0, dtype = tf$float32))
                      masked_neg_w <- tf$where(negative_w, x, tf$zeros_like(x))
                      masked_pos_w <- tf$where(negative_w, tf$zeros_like(x), x)
                      pos_w_scaled <-  self$long_position_size*tf$tanh(masked_pos_w)
                      neg_w_scaled <- self$short_position_size*tf$tanh(masked_neg_w)
                      pos_w_scaled <- tf$divide(pos_w_scaled, k_sum(tf$tanh(masked_pos_w), axis = 2L, keepdims = TRUE))
                      neg_w_scaled <- tf$divide(neg_w_scaled, k_sum(tf$tanh(masked_neg_w), axis = 2L, keepdims = TRUE))
                      constrained_w <- tf$where(negative_w, neg_w_scaled, pos_w_scaled)
                      
                      
                      return(constrained_w)
                    },
                    row_scale = function(x) {
                      
                      
                      row_mean <- tf$reduce_mean(x, 
                                                 axis = 1L, 
                                                 keepdims = TRUE)
                      
                      row_std <- tf$math$reduce_std(x, 
                                                    axis = 1L,
                                                    keepdims = TRUE)
                      
                      scaled_tensor <- tf$where(row_std > 0, (x - row_mean) / row_std, (x - row_mean))
                      
                      return(scaled_tensor)
                    },
                    build_model = function(n_stocks, rate, units_choice, activation_nl_i, activation_conc,
                                            learning_rate, activation_nl_ii, data, loss = sharpe_ratio_loss) {
                      
                      num_features <- ncol(data)
                      
                      # Building linear model input
                      layer_1 <- layer_input(shape = c(num_features), 
                                             name = "Linear_Input")
                      layer_linear <- layer_1 %>%
                        layer_dense(units = 12 * units_choice,
                                    activation = "linear",
                                    kernel_initializer = initializer_glorot_uniform(seed = 144), 
                                    name = "Linear_Layer") %>% 
                        layer_dropout(rate, name = "Drop_Out_Layer_Lin")%>%
                        layer_lambda(row_scale, name = "Cross-sectional_Normalisation_Layer_I")
                      
                      # Building non-linear model input
                      layer_2 <- layer_input(shape = c(num_features), 
                                             name = "Non_Linear_Input")
                      layer_nonlinear <- layer_2 %>%
                        layer_dense(units = units_choice * 12,
                                    activation = activation_nl_i,
                                    kernel_initializer = initializer_glorot_uniform(seed = 144),
                                    name = "Non_Linear_Layer_I") %>%
                        layer_dropout(rate, 
                                      name = "Drop_Out_Layer_Non_Lin") %>%
                        layer_dense(units = units_choice * 6,
                                    activation = activation_nl_ii,
                                    kernel_initializer = initializer_glorot_uniform(seed = 144),
                                    name = "Non_Linear_Layer_II") %>% 
                        layer_lambda(row_scale,
                                     name = "Cross-sectional_Normalisation_Layer_III")
                      
                      # Concatenating linear and non-linear inputs
                      concat <- layer_concatenate(list(layer_linear, layer_nonlinear), 
                                                  name = "Concatenation")
                      
                      # Building the rest of the model
                      layer_3 <- concat %>%
                        layer_dense(units = units_choice*3,
                                    activation = activation_conc,
                                    kernel_initializer = initializer_glorot_uniform(seed = 144), 
                                    name = "Concatenated_Layer_Non_Linear_III") %>% layer_lambda(row_scale)
                      
                      # Define complete model
                      output <- layer_3 %>%
                        layer_dense(units = n_stocks,
                                    activation = "linear",
                                    kernel_initializer = initializer_glorot_uniform(seed = 144),
                                    name = "Linear_Output_Layer") %>% layer_lambda(row_scale) %>%
                        layer_lambda(w_full_constraint_leverage,
                                     name = "Weights_Normalisation_Under_Constraints") 
                      
                      model <- keras_model(inputs = list(layer_1, layer_2), 
                                           outputs = output, 
                                           name = "Deep_Portfolio_Model")
                      model %>% compile(
                        loss = loss,
                        optimizer = optimizer_nadam(learning_rate = learning_rate)
                      )
                      
                      return(model)
                    },
                    
                    build_simple_model = function(n_stocks, rate, units_choice, activation_nl1,
                                                   learning_rate, activation_nl2, data, loss = sharpe_ratio_loss) {
                      
                      num_features <- ncol(data)
                      # Building model
                      model_sequential <- keras_model_sequential()
                      model_sequential %>%
                        layer_dense(units = units_choice*25,
                                    activation = activation_nl1,
                                    kernel_initializer = initializer_glorot_uniform(seed = 144)
                        )  %>% 
                        layer_dense(units = units_choice*12,
                                    activation = activation_nl2,
                                    kernel_initializer = initializer_glorot_uniform(seed = 144)) %>% 
                        layer_dropout(rate)%>%
                        layer_dense(units = units_choice*5,
                                    activation = "tanh",
                                    kernel_initializer = initializer_glorot_uniform(seed = 144)) %>% 
                        layer_dense(units = n_stocks,
                                    activation = "linear",
                                    kernel_initializer = initializer_glorot_uniform(seed = 144)) %>%
                        layer_lambda(row_scale) %>%
                        layer_lambda(w_full_constraint_leverage)  # Full investment constraint with no leverage allowed
                      
                      model_sequential %>% compile(
                        loss = loss,
                        optimizer = optimizer_nadam(learning_rate = learning_rate)
                      )
                      
                      return(model_sequential)
                    },
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
                                                                      factor = 0.005, 
                                                                      patience = 25,
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
                              batch_size = 7,
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
                            negative_values <- self$short_position_size*negative_values/sum(negative_values)
                          }
                          
                          # Normalize positive values
                          if (length(positive_values) > 0) {
                            positive_values <- self$long_position_size*positive_values / sum(positive_values)
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

                        