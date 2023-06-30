EarlyStoppingAtNegSR(keras$callbacks$Callback) %py_class% {
  
  
  initialize <- function(patience = 0) {
    # call keras$callbacks$Callback$__init__(), so it can setup `self`
    super$initialize()
    self$patience <- patience
    # best_weights to store the weights at which the minimum loss occurs.
    self$best_weights <- NULL
  }
  
  on_train_begin <- function(logs = NULL) {
    # The number of epoch it has waited when loss is no longer minimum.
    self$wait <- 0
    # The epoch the training stops at.
    self$stopped_epoch <- 0
    # Initialize the best as infinity.
    self$best <- Inf
  }
  
  on_epoch_end <- function(epoch, logs = NULL) {
    current <- logs$loss
    target_loss <- -0.1
    if (current < target_loss) {
      self$stopped_epoch <- epoch
      self$model$stop_training <- TRUE
      cat(sprintf("Epoch %05d: early stopping\n", self$stopped_epoch + 1))
    } else {
      if (current < self$best) {
        self$best <- current
        self$wait <- 0
        # Record the best weights if current results is better (less).
        self$best_weights <- self$model$get_weights()
      } else {
        self$wait %<>% `+`(1)
        if (self$wait >= self$patience) {
          self$stopped_epoch <- epoch
          self$model$stop_training <- TRUE
          cat("Restoring model weights from the end of the best epoch.\n")
          self$model$set_weights(self$best_weights)
        }
      }
    }
  }
  
  on_train_end <- function(logs = NULL)
    if (self$stopped_epoch > 0)
      cat(sprintf("Epoch %05d: early stopping\n", self$stopped_epoch + 1))
  
}