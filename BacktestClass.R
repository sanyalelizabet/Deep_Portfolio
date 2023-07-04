#' Backtest Class
#'
#' This class represents a backtest object.
#'
#' @field data_wide The input data in wide format.
#' @field returns The matrix or data frame of returns.
#' @field dates The vector of dates
#' @field train_window The length of initial training.
#' @field val_window The length of validation window.
#' @field model The model used for prediction: "".
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
                    public = list(
                      data_wide = NULL,
                      returns = NULL,
                      dates = NULL,
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
                      FF_factors=NULL,
                      
                      initialize = function(# Data and settings
                        data_wide = NULL,
                        returns = NULL,
                        dates = NULL,
                        FF_factors = NULL,
                        stocks_preselected = TRUE,
                        
                        train_window = 60,
                        val_window = 60,
                        window = "expand",
                        
                        # Model configuration
                        model = "simple",
                        units_choice = 10,
                        activation_nl_i = "sigmoid",
                        activation_conc = "swish",
                        activation_nl_ii = "tanh",
                        learning_rate = 0.01,
                        epochs = 50,
                        rate = 0.1,
                        batch_size = 12,
                        
                        # Portfolio settings
                        short_position_size = 0.2,
                        long_position_size = 1.2,
                        transaction_cost = FALSE,
                        transaction_fee = 0.005  ) {
                        
                        
                        
                        self$data_wide <- data_wide
                        self$returns <- returns
                        self$FF_factors <- FF_factors
                        self$dates <- dates
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
                        self$first_test <- val_window + train_window
                        self$testing_period_len <-  nrow(data_wide)-train_window-val_window
                        self$train_end_periods_seq <- seq(train_window, (nrow(data_wide)-val_window)) 
                        self$weights <- NULL
                        
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
                        y_ret <- tf$cast(y_ret, dtype=tf$float16)  
                        y_pred <- tf$cast(y_pred, dtype=tf$float16) 
                        weighted_returns <- y_ret * y_pred
                        portfolio_return <- k_sum(weighted_returns, axis = 2L)
                        mean_return <- backend$mean(portfolio_return)
                        std_return <- backend$std(portfolio_return)
                        sharpe_ratio <- (mean_return + backend$constant(0.001, dtype = tf$float16)) / std_return
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
                        
                        y_ret <- tf$cast(y_ret, dtype=tf$float16)  
                        y_pred <- tf$cast(y_pred, dtype=tf$float16) 
                        
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
                        y_ret <- tf$cast(y_ret, dtype=tf$float16)  
                        y_pred <- tf$cast(y_pred, dtype=tf$float16) 
                        backend <-  backend()
                        has_nan <- tf$equal(y_ret, y_ret) 
                        has_nan <-  k_cast_to_floatx(has_nan)
                        
                        nan_ret_to_zero <- tf$where(tf$equal(y_ret, y_ret), y_ret, tf$zeros_like(y_ret) )
                        nan_w_to_zero <- tf$math$multiply(y_pred, has_nan)
                        nan_w_to_zero <- nan_w_to_zero/k_sum(nan_w_to_zero)
                        
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
                        std_return <- backend$std(portfolio_return)+ backend$constant(0.001, dtype = tf$float16)
                        sharpe_ratio <- (mean_return-transaction_cost) / (std_return)
                        
                        
                        return(-sharpe_ratio)
                      },
                      
                      w_full_constraint_leverage = function(x) {
                        x <- tf$cast(x, dtype=tf$float16)  
                        negative_w <- tf$math$less_equal(x, tf$constant(0, dtype = tf$float16))
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
                          layer_lambda(self$row_scale, name = "Cross-sectional_Normalisation_Layer_I")
                        
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
                          layer_lambda(self$row_scale,
                                       name = "Cross-sectional_Normalisation_Layer_III")
                        
                        # Concatenating linear and non-linear inputs
                        concat <- layer_concatenate(list(layer_linear, layer_nonlinear), 
                                                    name = "Concatenation")
                        
                        # Building the rest of the model
                        layer_3 <- concat %>%
                          layer_dense(units = units_choice*3,
                                      activation = activation_conc,
                                      kernel_initializer = initializer_glorot_uniform(seed = 144), 
                                      name = "Concatenated_Layer_Non_Linear_III") %>% layer_lambda(self$row_scale)
                        
                        # Define complete model
                        output <- layer_3 %>%
                          layer_dense(units = n_stocks,
                                      activation = "linear",
                                      kernel_initializer = initializer_glorot_uniform(seed = 144),
                                      name = "Linear_Output_Layer") %>% layer_lambda(self$row_scale) %>%
                          layer_lambda(self$w_full_constraint_leverage,
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
                                                    learning_rate, activation_nl2, data, loss = self$sharpe_ratio_loss) {
                        
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
                          layer_lambda(self$row_scale) %>%
                          layer_lambda(self$w_full_constraint_leverage) 
                        
                        model_sequential %>% compile(
                          loss = loss,
                          optimizer = optimizer_nadam(learning_rate = learning_rate)
                        )
                        
                        return(model_sequential)
                      },
                      build_lstm_model = function(n_stocks, rate, units_choice,
                                                   learning_rate, data, loss = sharpe_ratio_loss,timesteps=12 ) {
                        
                        num_features <- dim(data)[3]
                        model_sequential <- keras_model_sequential() 
                        model_sequential%>%
                          layer_cudnn_lstm(units = self$units_choice*15, 
                                           input_shape = c(timesteps, num_features),
                                           return_sequences = TRUE
                          ) %>%
                          layer_cudnn_lstm(units = self$units_choice*4, 
                                           input_shape = c(timesteps, num_features),
                                           return_sequences = FALSE
                          ) %>%
                          layer_dense(units = n_stocks,
                                      activation = "linear")  %>%  layer_lambda(self$row_scale) %>%
                          layer_lambda(self$w_full_constraint_leverage)
                        
                        model_sequential %>% compile(
                          loss = loss,
                          optimizer = optimizer_adam(learning_rate =as.double(self$learning_rate))
                        )
                        
                        return(model_sequential)
                      },
                      
                      backtest = function(t, data_wide, returns, model="simple",
                                          window="expand", stocks_preselected=TRUE, 
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
                        
                        n_stock_selected <- ncol(returns)
                        
                        # Data subset
                        if (window=="fixed"| model == "lstm"){
                          
                          window_data <- as.matrix(data_wide[(t-self$training_window+1):t,])
                          window_returns <- as.matrix(returns[(t-self$training_window+1):t,])
                          val_window_data <- as.matrix(data_wide[t:(t+self$val_window-1),])
                          val_returns <- as.matrix(returns[t:(t+self$val_window-1),])
                          data_predict <- as.matrix(data_wide[(t+self$val_window),])
                          
                        } else {
                          
                          first <- t%%self$batch_size+1
                          window_data <- as.matrix(data_wide[first:t,])
                          window_returns <- as.matrix(returns[first:t,])
                          val_window_data <- as.matrix(data_wide[t:(t+self$val_window-1),])
                          val_returns <- as.matrix(returns[t:(t+self$val_window-1),])
                          data_predict <- as.matrix(data_wide[(t+self$val_window),])
                          
                          
                        }
                        
                        #Training details
                        early_stopping <- callback_early_stopping(monitor = c('val_loss'),
                                                                  patience = 50,
                                                                  restore_best_weights = TRUE
                        )
                        
                        lr_callback <- callback_reduce_lr_on_plateau( monitor = 'val_loss',
                                                                      factor = 0.75, 
                                                                      patience = 25,
                                                                      verbose = 1,
                                                                      mode = 'auto'
                        )
                        #Specifying the loss function
                        if (stocks_preselected) {
                          if (anyNA(returns)) {
                            stop("NA values found in returns. Please ensure returns do not have missing values.")
                          }
                          loss_fun <- self$sharpe_ratio_loss
                          
                          if (transaction_cost){
                            loss_fun <- self$sharpe_ratio_tc_loss
                          }
                          
                          
                        } else {
                          loss_fun <- self$sharpe_ratio_loss_na_check
                          
                          if (transaction_cost){
                            loss_fun <- self$sharpe_ratio_tc_loss_na_check
                          }
                        }
                        #Model Build/Fitting/Predict
                        if (model=="simple"){
                          
                          model <- self$build_simple_model(n_stock_selected, self$rate, self$units_choice, 
                                                      self$activation_nl_i, self$learning_rate, self$activation_nl_ii, 
                                                      data_wide,loss_fun )
                          
                          model %>% fit( x = window_data, 
                                         y = window_returns, 
                                         epochs = self$epochs, 
                                         batch_size = self$batch_size, 
                                         verbose = 2,  
                                         validation_data = list(val_window_data, val_returns),
                                         shuffle=FALSE,
                                         callbacks = list(early_stopping,lr_callback)
                          )
                          
                          prediction_w_t <- predict(model, data_predict)
                          
                        } else if (model == "complex"){
                            
                            model <- self$build_model(n_stock_selected, self$rate, self$units_choice, 
                                                 self$activation_nl_i,self$activation_conc,
                                                 self$learning_rate,self$activation_nl_ii, 
                                                 data_wide,loss_fun)
                            
                            model %>% fit( x = list(window_data, window_data), 
                                           y = window_returns, 
                                           epochs = self$epochs, 
                                           batch_size = self$batch_size, 
                                           verbose = 2,  
                                           validation_data = list(list(val_window_data, val_window_data), val_returns),
                                           shuffle=FALSE,
                                           callbacks = list(early_stopping, lr_callback)
                            )
                            
                            prediction_w_t <- predict(model, list(data_predict,data_predict))
                          } else if (model == "lstm"){
                            # Building sequences
                            data_lstm_train <- timeseries_dataset_from_array(window_data, window_returns, 12)
                            lstm_features_train<- data_lstm_train$features
                            lstm_features_target <- data_lstm_train$target 
                            
                            data_lstm_val <- timeseries_dataset_from_array(val_window_data, val_returns, 12)
                            lstm_features_val<- data_lstm_val$features
                            lstm_target_val <- data_lstm_val$target
                            
                            data_predict <- as.matrix(data_wide[(t+self$val_window-11):(t+self$val_window),])
                            return_observed <- as.matrix(returns[(t+self$val_window-11):(t+self$val_window),])
                            data_lstm_predict <- timeseries_dataset_from_array(data_predict, return_observed, 12)
                            lstm_feat_pred <- data_lstm_predict$features
                            
                            model <- self$build_lstm_model(n_stock_selected, self$rate, self$units_choice,
                                                      self$learning_rate, lstm_features_train, loss = loss_fun,timesteps=12)
                            
                            model %>% fit(
                              lstm_features_train ,lstm_features_target ,
                              epochs = self$epochs,
                              batch_size = 7,
                              validation_data = list(lstm_features_val, lstm_target_val),
                              callbacks = list(early_stopping, lr_callback)
                            )
                            
                            prediction_w_t <- predict(model, lstm_feat_pred)
                            
                          }
                        
                        
                        if (stocks_preselected) {
                          # If stocks are preselected, the predicted weights are returned as they are.
                          return(prediction_w_t)
                          
                        } else {
                          return_data_predict <- as.matrix(returns[(t+self$val_window),])
                          
                          stocks_available  <- is.na(return_data_predict)
                          # If stocks are not preselected, any stocks with unavailable returns are assigned weights of 0.
                          # The predicted weights are then normalized again so that they sum to defined size of short and long position.
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
                            positive_values <- self$long_position_size*positive_values/sum(positive_values)
                          }
                          
                          # Combine normalized values
                          normalized_prediction_w_t <- prediction_w_t
                          normalized_prediction_w_t[prediction_w_t < 0] <- negative_values
                          normalized_prediction_w_t[prediction_w_t > 0] <- positive_values
                          
                          return(normalized_prediction_w_t)
                        }
                        
                      },
                      apply_backtest_to_periods = function() {
                        if (self$model=="all"){
                          all_models <- c("simple", "complex", "lstm")
                          
                          self$weights <- list()
                          
                          for (i in seq_along(all_models)) {
                            model_n <- all_models[i]
                            print(model_n)
                            
                            results <- purrr::map(self$train_end_periods_seq, self$backtest, 
                                                  self$data_wide, self$returns,model = model_n, 
                                                  window = self$window, stocks_preselected = self$stocks_preselected, 
                                                  transaction_cost = self$transaction_cost)
                            
                            model_weights <- self$extract_weights(results)
                            
                            self$weights <-  append(self$weights, list(model_weights))
                            names(self$weights)[i] <- model_n
                          }
                        } else {
                          
                          results <- purrr::map(self$train_end_periods_seq, self$backtest, 
                                                self$data_wide, self$returns, model = self$model, 
                                                window = self$window, stocks_preselected = self$stocks_preselected, 
                                                transaction_cost = self$transaction_cost)
                          self$weights <- append(self$weights, list(results))
                          names(self$weights)[1] <- self$model
                        }
                        
                        
                        return(self$weights)
                      },
                      
                      extract_weights = function(backt_object) {
                        return(do.call(rbind, lapply(backt_object, unlist)))
                      },
                      
                      calculate_turnover = function(weights, test_returns) {
                        
                        weighted_returns <- test_returns * weights
                        portfolio_return <- rowSums(weighted_returns)
                        
                        net_return <- test_returns + 1
                        prior_weights <- weights * net_return
                        
                        weights_to_rebalance <- weights[1:(nrow(weights)-1), ]
                        prior_weights_stride <- prior_weights[1:(nrow(weights)-1), ]
                        turn <- rowSums(abs(weights_to_rebalance - prior_weights_stride)) / rowSums(prior_weights_stride)
                        return(mean(turn)) 
                      },
                      
                      generate_performance_metrics = function() {
                        
                        first <- self$train_end_periods_seq[1]+self$val_window
                        last <- self$train_end_periods_seq[length(self$train_end_periods_seq)]+self$val_window
                        dates_testing <- sort(self$dates)[first:last]
                        FF_factors_test <-  dplyr::filter(self$FF_factors,  self$FF_factors[[1]] %in% dates_testing)
                        test_smpl_returns <- as.matrix(self$returns[first:last, ])
                        
                        if (self$stocks_preselected==FALSE) {
                          test_smpl_returns <- replace(test_smpl_returns, is.na(test_smpl_returns), 0)
                          
                        } 
                        
                        turn <-  unlist(purrr::map(self$weights, self$calculate_turnover, test_smpl_returns))
                        
                        pf_returns <-  purrr::map(self$weights, function(x) rowSums(x * test_smpl_returns) )
                        pf_ret_excess <- purrr::map(pf_returns, function(x) x-FF_factors_test$RF)
                        
                        avg_return <- unlist(purrr::map(pf_returns, function(x) mean(x) ))
                        avg_return_excess <-  unlist(purrr::map(pf_ret_excess, function(x) mean(x) ))
                        sd_return <-  unlist(purrr::map(pf_returns, function(x) sd(x) ))
                        
                        sharpe_ratio <- avg_return_excess/ sd_return
                        sharpe_ratio_tc_adj <-  (avg_return_excess - self$transaction_fee * turn) / sd_return
                        
                        max_weight <- unlist(purrr::map(self$weights, function(x) max(x) ))
                        min_weight <- unlist(purrr::map(self$weights, function(x) min(x) ))
                        
                        avg_neg_weights <- unlist(purrr::map(self$weights, function(x) mean(x[x < 0]) ))
                        avg_pos_weights <- unlist(purrr::map(self$weights, function(x) mean(x[x > 0]) ))
                        
                        frac_neg_weights <- unlist( purrr::map(self$weights, function(x) mean(apply(x, 1, function(row) sum(row[row < 0]))))) 
                        frac_pos_weights <- unlist( purrr::map(self$weights, function(x) mean(apply(x, 1, function(row) sum(row[row > 0])))))
                        
                        # Prepare data.frame for linear reg models
                        returns_excess_df <- as.data.frame(pf_ret_excess)
                        models <- colnames(returns_excess_df)
                        returns_excess_df$date <- FF_factors_test$date
                        FF_factors_test <- left_join(FF_factors_test, returns_excess_df)
                        
                        CAPM_alpha_results <- data.frame(model = character(),
                                                         alpha_CAPM = numeric(),
                                                         alpha_CAPM_t = numeric(),
                                                         alpha_CAPM_p = numeric(),
                                                         stringsAsFactors = FALSE)
                        
                        FF3_alpha_results <- data.frame(model = character(),
                                                        alpha_FF3 = numeric(),
                                                        alpha_FF3_t = numeric(),
                                                        alpha_FF3_p = numeric(),
                                                        stringsAsFactors = FALSE)
                        
                        for (model in models ) {
                          
                          regres_form <- lm(as.formula(paste(model, "~ Mkt.RF")), data = FF_factors_test)
                          alpha_CAPM <- coef(regres_form)[1]
                          alpha_CAPM_t <- coeftest(regres_form, vcov = NeweyWest(regres_form))[1, 3]
                          alpha_CAPM_p <- coeftest(regres_form, vcov = NeweyWest(regres_form))[1, 4]
                          
                          # Add results to the data frame
                          CAPM_alpha_results <- rbind(CAPM_alpha_results, data.frame(model = model,
                                                                                     alpha_CAPM = alpha_CAPM,
                                                                                     t = alpha_CAPM_t,
                                                                                     p = alpha_CAPM_p))
                        }
                        
                        rownames(CAPM_alpha_results) <- NULL
                        
                        for (model in models ) {
                          
                          regres_form <- lm(as.formula(paste(model, "~ Mkt.RF+ SMB + HML")), data = FF_factors_test)
                          alpha_FF3 <- coef(regres_form)[1]
                          alpha_FF3_t <- coeftest(regres_form, vcov = NeweyWest(regres_form))[1, 3]
                          alpha_FF3_p <- coeftest(regres_form, vcov = NeweyWest(regres_form))[1, 4]
                          
                          # Add results to the data frame
                          FF3_alpha_results <- rbind(FF3_alpha_results, data.frame(model = model,
                                                                                   alpha_FF3 = alpha_FF3,
                                                                                   t = alpha_FF3_t,
                                                                                   p = alpha_FF3_p))
                        }
                        
                        rownames(FF3_alpha_results) <- NULL
                        
                        table_data1 <- t(data.frame(
                          avg_return,
                          sd_return,
                          sharpe_ratio,
                          sharpe_ratio_tc_adj,
                          turn,
                          max_weight,
                          min_weight,
                          avg_neg_weights,
                          avg_pos_weights,
                          frac_neg_weights,
                          frac_pos_weights
                        ))
                        
                        rownames(table_data1) <- c(
                          "Average return",
                          "SD return",
                          "Sharpe ratio",
                          "Sharpe ratio TC Adj.",
                          "Average Turnover",
                          "Max. weight",
                          "Min. weight",
                          "Avg. negative weights",
                          "Avg. positive weights",
                          "Avg. fraction of negative weights",
                          "Avg. fraction of positive weights"
                        )
                        
                        
                        # Display the tables side by side
                        table1 <- knitr::kable(
                          table_data1,booktabs = TRUE, valign = 't', digits = 4,
                          caption = 'Performance Metrics.'
                        ) %>%
                          kable_styling(bootstrap_options = c("striped"))
                        
                        table2 <-  kable(CAPM_alpha_results) %>%
                          kable_styling(full_width = FALSE, position = "float_left")
                        kable(FF3_alpha_results) %>%
                          kable_styling(full_width = FALSE, position = "left")%>%
                          kable_styling(bootstrap_options = c("striped"))
                        
                        
                        return(list(statistics = table1, alphas = table2))
                      }
                      
                      
                      
                    )
)

