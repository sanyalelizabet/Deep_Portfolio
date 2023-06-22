winsorize <- function(x, cut) {
  x <- replace(
    x,
    x > quantile(x, 1 - cut, na.rm = T),
    quantile(x, 1 - cut, na.rm = T)
  )
  x <- replace(
    x,
    x < quantile(x, cut, na.rm = T),
    quantile(x, cut, na.rm = T)
  )
  return(x)
}

impute_subset <- function(prefix, data, FUN = median) {
  imputed_subset <- data%>% 
    select(starts_with(prefix)) %>% 
    rowwise() %>% 
    mutate(median_value = median(c_across(starts_with(prefix)), na.rm = TRUE))%>%  
    mutate(across(starts_with(prefix), ~ if_else(is.na(.), median_value, .))) %>%
    select(-median_value)
  return(imputed_subset)
}

impute_rowwise <- function(data, FUN = median, feature_prefixes) {
  plan(multisession, workers = 15)
  imputed_data_list <- future_map(feature_prefixes, ~ impute_subset(.x, data, FUN))
  combined_data <- bind_cols(imputed_data_list)
  return(combined_data)
}


process_data <- function(data, stock_id_column, feature_names = NULL, label_name, use_max_stock_days = TRUE) {
  stock_ids <- levels(as.factor(data[[stock_id_column]]))
  
  if (use_max_stock_days) {
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
    dplyr::select(!!sym(stock_id_column), !!sym(date_column), features, label_name) 
  
  features_wide <- data_selection %>%
    select(-!!sym(label_name)) %>%
    pivot_wider(names_from = !!sym(stock_id_column), values_from = features) %>%
    arrange(!!sym(date_column)) %>%
    select(-!!sym(date_column)) 
  
  
  #features_wide <- impute_rowwise(features_wide, FUN=median, feature_prefixes=features)
  
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
    select({{ label_name }}, !!sym(stock_id_column), !!sym(date_column)) %>%
    pivot_wider(names_from = !!sym(stock_id_column), values_from =!!sym(label_name)) %>%
    arrange(!!sym(date_column)) %>%
    select(-!!sym(date_column))
  
  return(list(features_wide = features_wide, returns_winsorized = returns_winsorized, returns = returns, dates = dates))
}



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

impute_returns <- function(returns) {
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