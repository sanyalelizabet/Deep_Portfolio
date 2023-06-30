#' Build a complex model with linear and non-linear inputs
#'
#' @param n_stocks The number of stocks
#' @param rate The dropout rate
#' @param units_choice The choice of units for dense layers
#' @param activation_nl_i The activation function for the first non-linear dense layer
#' @param activation_conc The activation function for the concatenation layer
#' @param learning_rate The learning rate for the optimizer
#' @param activation_nl_ii The activation function for the second non-linear dense layer
#' @param data The input features data for the model
#' @param loss The loss function (default: sharpe_ratio_loss)
#'
#' @return The built and compiled model


build_model <- function(n_stocks, rate, units_choice, activation_nl_i, activation_conc,
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
}


#' Build a simple sequential model
#'
#' @param n_stocks The number of stocks
#' @param rate The dropout rate
#' @param units_choice The choice of units for dense layers
#' @param activation_nl1 The activation function for the first dense layer
#' @param learning_rate The learning rate for the optimizer
#' @param activation_nl2 The activation function for the second dense layer
#' @param data The input data for the model
#' @param loss The loss function (default: sharpe_ratio_loss)
#'
#' @return The built and compiled sequential model
build_simple_model <- function(n_stocks, rate, units_choice, activation_nl1,
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
}


build_lstm_model <- function(n_stocks, rate, units_choice,
                               learning_rate, data, loss = sharpe_ratio_loss,timesteps=12) {
  
  num_features <- dim(data)[3]
  model_sequential <- keras_model_sequential() 
  model_sequential%>%
    layer_cudnn_lstm(units = units_choice*25, 
               input_shape = c(timesteps, num_features),
               return_sequences = FALSE
    ) %>%
    layer_dense(units = n_stocks,
                activation = "linear")  %>%  layer_lambda(row_scale)  %>%
    layer_lambda(w_full_constraint_leverage)
  
  model_sequential %>% compile(
    loss = loss,
    optimizer = optimizer_adam(learning_rate =as.double(learning_rate))
  )
  
  return(model_sequential)
}
    
