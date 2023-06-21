build_model <- function(rate, units_choice, activation_nl_i,activation_conc,learning_rate, activation_nl_ii, data ){
  
  
  num_features <- ncol(data)
  #Building model
  
  layer_1 <- layer_input(shape = c(num_features), 
                         name = "Linear_Model_Input")
  
  layer_linear <-  layer_1 %>% 
    layer_dense(units = length(features)*units_choice, 
                activation = "linear",
                kernel_initializer = initializer_glorot_uniform(seed=145),
    ) %>%
    layer_batch_normalization(scale = TRUE, center = TRUE) %>%
    layer_dropout(rate = 0.2) 
  
  
  layer_2 <- layer_input(shape = c(num_features), name = "Non-Linear_Model_Input")
  layer_nonlinear <- layer_2 %>% layer_dense(units = units_choice*3,
                                             activation = activation_nl_i,
                                             kernel_initializer = initializer_glorot_uniform(seed=142)
                                             
  )  %>% layer_dropout(rate = rate)  
  layer_nonlinear <- layer_nonlinear %>%  layer_dense(units = units_choice*2,
                                                      activation = activation_nl_ii,
                                                      kernel_initializer = initializer_glorot_uniform(seed=141))
  
  
  concat <- layer_concatenate(list(layer_linear, layer_nonlinear))
  layer_3 <- concat %>%
    layer_dense(units = units_choice, 
                activation = activation_conc, 
                kernel_initializer = initializer_glorot_uniform(seed=145)
    )  %>% 
    layer_batch_normalization(scale = TRUE, center = TRUE)
  #Define complete model
  output <- layer_3 %>%
    layer_dense(units = length(stock_ids_short), 
                activation = "linear",
                kernel_initializer = initializer_glorot_uniform(seed=145))  %>% 
    layer_lambda(w_full_constraint_leverage) # full investment constrain with no leverage allowed
  
  model <- keras_model(inputs = list(layer_1, layer_2), outputs = output)
  model %>% compile(
    loss = sharpe_ratio_loss,
    optimizer = optimizer_nadam(learning_rate = learning_rate,clipnorm=1)
    
  )
  return(model)
} 




build_simple_model <- function(rate, units_choice, activation_nl_i, learning_rate, activation_nl_ii, data ){
  
  
  num_features <- ncol(data)
  #Building model
  
  model_sequential <- keras_model_sequential()
  model_sequential  %>%
    layer_dense(units = units_choice * 3,
                activation = activation_nl_i,
                kernel_initializer = initializer_glorot_uniform(seed = 145)) %>%
    layer_dropout(rate = rate) %>%
    layer_dense(units = units_choice * 2,
                activation = activation_nl_ii,
                kernel_initializer = initializer_glorot_uniform(seed = 142)) %>%
    layer_batch_normalization(scale = TRUE, center = TRUE) %>%
    layer_dense(units = length(stock_ids_short),
                activation = "linear",
                kernel_initializer = initializer_glorot_uniform(seed = 143)) %>%
    layer_lambda(w_full_constraint_leverage) # Full investment constraint with no leverage allowed
  
  model_sequential %>% compile(
    loss = sharpe_ratio_loss,
    optimizer = optimizer_nadam(learning_rate = learning_rate, clipnorm = 1)
  )
    
  return(model_sequential)
} 