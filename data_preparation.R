#' Winsorize a vector by trimming extreme values
#'
#' @param vector The input vector to be winsorized
#' @param trim_fraction The fraction of extreme values to be trimmed from both ends
#'
#' @return The winsorized vector
winsorize <- function(vector, trim_fraction) {
  vector <- replace(
    vector,
    vector > quantile(vector, 1 - trim_fraction),
    quantile(vector, 1 - trim_fraction)
  )
  vector <- replace(
    vector,
    vector < quantile(vector, trim_fraction),
    quantile(vector, trim_fraction)
  )
  return(vector)
}

#' Impute missing values for a subset of columns based on a prefix
#'
#' @param data The input data frame
#' @param prefix The column name prefix to select columns
#' @param FUN The imputation function to use (default: median)
#'
#' @return A data frame with missing values imputed using the specified function
impute_subset <- function(prefix, data, FUN = median) {
  imputed_subset <- data %>% 
    select(starts_with(prefix)) %>% 
    rowwise() %>% 
    mutate(median_value = median(c_across(starts_with(prefix)), na.rm = TRUE)) %>%  
    mutate(across(starts_with(prefix), ~ if_else(is.na(.), median_value, .))) %>%
    select(-median_value)
  return(imputed_subset)
}

#' Impute missing values row-wise for multiple subsets of columns
#'
#' @param data The input data frame
#' @param FUN The imputation function to use (default: median)
#' @param feature_prefixes A vector of column name prefixes for subsets of columns
#'
#' @return A data frame with missing values imputed for all subsets of columns
impute_rowwise <- function(data, FUN = median, feature_prefixes) {
  plan(multisession, workers = 15)
  imputed_data_list <- future_map(feature_prefixes, ~ impute_subset(.x, data, FUN))
  combined_data <- bind_cols(imputed_data_list)
  return(combined_data)
}

#' Process data for analysis
#'
#' @param data The input data frame
#' @param stock_id_column The column name for stock IDs
#' @param feature_names A vector of feature column names (default: NULL). All columns except label will be used
#' @param label_name The column name for the label
#' @param use_full_data_stocks Boolean indicating whether to use only stock IDs with full data (default: TRUE)
#'
#' @return A list containing processed data: features_wide, returns_winsorized, returns, dates
process_data <- function(data, stock_id_column, feature_names = NULL,
                         label_name, use_full_data_stocks = TRUE) {
  stock_ids <- levels(as.factor(data[[stock_id_column]]))
  
  if (use_full_data_stocks) {
    stock_days <- data %>%
      group_by(!!sym(stock_id_column)) %>%
      summarize(nb = n())
    stock_ids_short <- stock_ids[which(stock_days$nb == max(stock_days$nb))]
  } else {
    stock_ids_short <- stock_ids
  }
  
  # Find the date column automatically
  date_column <- colnames(data)[sapply(data, function(x) class(x) == "Date")]
  
  if (is.null(date_column)) {
    stop("No date column found in the dataset.")
  }
  
  dates <- unique(data[[date_column]])
  
  if (is.null(feature_names)) {
    features <- setdiff(colnames(data), c(label_name, stock_id_column, date_column))
  } else {
    features <- feature_names
  }
  
  data_selection <- data %>%
    filter(!!sym(stock_id_column) %in% stock_ids_short) %>%
    dplyr::select(!!sym(stock_id_column), !!sym(date_column), all_of(features),all_of(label_name) ) 
  
  features_wide <- data_selection %>%
    select(-!!sym(label_name)) %>%
    pivot_wider(names_from = !!sym(stock_id_column), values_from = features) %>%
    arrange(!!sym(date_column)) %>%
    select(-!!sym(date_column)) 
  
  returns_winsorized <- data_selection %>%
    select(!!sym(label_name), !!sym(stock_id_column), !!sym(date_column)) %>%
    mutate(across(
      c(!!sym(label_name)),
      ~ winsorize(., 0.01)
      )) %>%
    pivot_wider(names_from = !!sym(stock_id_column), values_from = !!sym(label_name)) %>%
    arrange(!!sym(date_column)) %>%
    select(-!!sym(date_column))
  
  returns <- data_selection %>%
    select(!!sym(label_name), !!sym(stock_id_column), !!sym(date_column)) %>%
    pivot_wider(names_from = !!sym(stock_id_column), values_from =!!sym(label_name)) %>%
    arrange(!!sym(date_column)) %>%
    select(-!!sym(date_column))
  
  return(list(features_wide = features_wide, returns_winsorized = returns_winsorized, returns = returns, dates = dates))
}

#' Calculate NA statistics for a vector of returns
#'
#' @param returns The vector of returns
#'
#' @return A matrix containing the start indices and consecutive NA counts

calculate_na_stats <- function(returns) {
  na_indices <- which(is.na(returns))  # Find the indices of NA values
  na_stats <- NULL
  
  if (length(na_indices) == 0) {
    return(na_stats)  # No missing values, return NULL
  }
  
  if (length(na_indices) == 1) {
    na_stats <- rbind(na_stats, c(na_indices, 1))
    return(na_stats)
  }
  
  first_index <- na_indices[1]
  count <- 1
  prev_index <- first_index  # Initialize prev_index
  
  for (i in 2:length(na_indices)) {
    curr_index <- na_indices[i]
    
    if (curr_index == prev_index + 1) {
      count <- count + 1
    } else {
      na_stats <- rbind(na_stats, c(first_index, count))
      first_index <- curr_index
      count <- 1
    }
    prev_index <- curr_index
  }
  
  na_stats <- rbind(na_stats, c(first_index, count))  # Add the last occurrence
  
  return(na_stats)
}

#' Impute missing values in a vector of returns
#'
#' @param returns The vector of returns
#'
#' @return The imputed vector of returns
impute_returns <- function(returns) {
  if (!is.vector(returns)) {
    stop("Input 'returns' must be a vector.")
  }
  
  na_stats <- calculate_na_stats(returns)
  
  if (is.null(na_stats)) {
    return(returns)  # No missing values, return original vector
  }
  imputed_returns <- returns
  first_index <- na_stats[1, 1]
  occurences_end <- na_stats[nrow(na_stats), 2]
  last_index <- na_stats[nrow(na_stats), 1]
    
  if (first_index != 1) {
      
      temp <- imputed_returns[first_index:(last_index-1)]
      temp[is.na(temp)] <- 0
      imputed_returns[first_index:(last_index-1)] <- temp
      
      if ((last_index + occurences_end-1) != length(imputed_returns)) {
        imputed_returns[(last_index + 1):length(imputed_returns)] <- 0
      }
    } else {
      
      if (nrow(na_stats)!=1) {
      start <- na_stats[1, 2]
      temp <- imputed_returns[start:(last_index-1)]
      temp[is.na(temp)] <- 0
      imputed_returns[start:(last_index-1)] <- temp
      
      if ((last_index + occurences_end-1) != length(imputed_returns)) {
        imputed_returns[(last_index + 1):length(imputed_returns)] <- 0
            }
        }
    }
  
  return(imputed_returns)
}

#' Generate a time series dataset from an array.
#'
#' This function takes an array of data and its corresponding targets and generates a time series dataset with specified sequence length, stride, and sampling rate.
#'
#' @param data An array containing the input data.
#' @param targets An array containing the target data.
#' @param sequence_length The length of each sequence in the dataset.
#' @param sequence_stride The stride between consecutive sequences.
#' @param sampling_rate The rate at which the data is sampled.
#' @param shuffle Logical value indicating whether to shuffle the generated sequences.
#' @param seed An optional seed for the random shuffling process.
#' @param start_index The index to start generating sequences from. If not provided, it defaults to 1.
#' @param end_index The index to stop generating sequences at. If not provided, it defaults to the last index of the data.
#'
#' @return A list containing the generated features and target arrays.
#'
timeseries_dataset_from_array <- function(
    data,
    targets,
    sequence_length,
    sequence_stride = 1,
    sampling_rate = 1,
    shuffle = FALSE,
    seed = NULL,
    start_index = NULL,
    end_index = NULL
) {
  if (!is.null(start_index)) {
    if (start_index < 0) {
      stop(paste("`start_index` must be 0 or greater. Received:", start_index))
    }
    if (start_index >= length(data)) {
      stop(paste("`start_index` must be lower than the length of the data. Received:",
                 start_index, "for data of length", length(data)))
    }
  }
  
  if (!is.null(end_index)) {
    if (!is.null(start_index) && end_index <= start_index) {
      stop(paste("`end_index` must be higher than `start_index`. Received:",
                 "start_index =", start_index, "and end_index =", end_index))
    }
    if (end_index >= length(data)) {
      stop(paste("`end_index` must be lower than the length of the data. Received:",
                 "end_index =", end_index, "for data of length", length(data)))
    }
    if (end_index <= 0) {
      stop(paste("`end_index` must be higher than 0. Received:", end_index))
    }
  }
  
  if (sampling_rate <= 0) {
    stop(paste("`sampling_rate` must be higher than 0. Received:", sampling_rate))
  }
  if (sampling_rate >= length(data)) {
    stop(paste("`sampling_rate` must be lower than the length of the data. Received:",
               sampling_rate, "for data of length", length(data)))
  }
  if (sequence_stride <= 0) {
    stop(paste("`sequence_stride` must be higher than 0. Received:", sequence_stride))
  }
  if (sequence_stride >= length(data)) {
    stop(paste("`sequence_stride` must be lower than the length of the data. Received:",
               sequence_stride, "for data of length", length(data)))
  }
  
  if (is.null(start_index)) {
    start_index <- 1
  }
  if (is.null(end_index)) {
    end_index <- nrow(data)
  }
  
  num_seqs <- end_index - start_index - (sequence_length * sampling_rate) + 1
  
  if (!is.null(targets)) {
    num_seqs <- min(num_seqs, length(targets))
  }
  
  if (num_seqs < 2147483647) {
    index_dtype <- "integer32"
  } else {
    index_dtype <- "integer64"
  }
  
  start_positions <- seq(1, num_seqs, by = sequence_stride)
  if (shuffle) {
    set.seed(seed)
    start_positions <- sample(start_positions)
  }
  
  sequence_length <- as.integer(sequence_length)
  sampling_rate <- as.integer(sampling_rate)
  
  data_features <- lapply(start_positions, function(start) {
    fin <- array(0, dim = c(1, sequence_length,ncol(data)))
    end <- start + (sequence_length * sampling_rate) - sampling_rate
    fin[1,,] <- as.matrix(data[start:end, ])
  })
  
  target <- lapply(start_positions, function(start) {
    fin <- array(0, dim = c(1, ncol(targets)))
    end <- start + (sequence_length * sampling_rate) - sampling_rate
    fin[1,] <- as.matrix(targets[end,])
    fin
  })
  array_features <-abind::abind(data_features, along = 3)
  array_features <- aperm(array_features, c(3, 1, 2))
  array_target <-   abind::abind(target, along = 3)
  array_target <- aperm(array_target, c(3, 1, 2))
  return(list(features = array_features, target = array_target))
}
