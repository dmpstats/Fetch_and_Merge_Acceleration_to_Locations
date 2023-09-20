library('move2')
library('lubridate')
library("units")
library("dplyr")
library("tidyr")
library("purrr")
library("stringr")
library("assertthat")

## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

rFunction = function(data, usr, pwd){
  
  # Set movebank login credentials
  options("keyring_backend"="env")
  movebank_store_credentials(username = usr, password = pwd)
  
  # 1. Get study IDs and individual IDs in input dataset -----------------------
  # TODO
  
  # 2. Find which individuals have acc data available --------------------------
  # TODO
  
  # 3. Download Acc data -------------------------------------------------------
  # TODO
  
  # 4. Preprocess ACC data
  #acc_processed <- preprocess_acc("dummy")
  
  
  # 5. Bind ACC data to input data 
  # TODO
  

  # provide my result to the next app in the MoveApps workflow
  return(result)
}




#' /////////////////////////////////////////////////////////////////////////////
preprocess_acc <- function(.data){
  
  # input validation -------------------------------------
  # check if time is in ascending order
  assertthat::assert_that(mt_is_time_ordered(.data))
  
  # check if data is for a single individual
  if(length(unique(mt_track_id(.data))) > 1){
    stop("Unable to pre-process ACC data - provided data must be from a single individual")
  } 
  
  # store move2 data and identifiers, to retrieve later --------------------------
  # track data
  track_dt <- mt_track_data(.data)
  # time column name, 
  tm_col <- mt_time_column(.data)
  # track id column
  id_col <- mt_track_id_column(.data)
  
  
  # Standardize format ---------------------------------
  
  # Depending on format type (i.e. nested or plain), provided data reshaped to
  # have bursts of ACC nested in a list-column named `acc_bursts`, i.e. with one
  # burst per row. Type (raw or non-raw) and origin (eobs or non-eobs) of acc
  # data is also stored.
  #
  # NOTE: Code assumes types of data and tag origin are mutually exclusive at the
  # animal level - i.e., .data can only contain either raw or non-raw acc, from
  # eobs or non-eobs tags
  
  # get column names
  cols <- names(.data)
  
  # get format of acc data
  acc_format <- get_acc_format(.data)
  
  if(acc_format == "nested"){
    
    # get column names with nested acc and axes specification
    acc_col <- grep("(A|a)ccelerations(_raw)?\\b", cols, value = TRUE)
    axes_col <- grep("acceleration_axes", cols, value = TRUE)
    
    # convert character-type acc data to matrix and store in list-column
    .data <- .data |> 
      dplyr::mutate(
        acc_bursts = purrr::map2(.data[[acc_col]], .data[[axes_col]], acc_string_to_matrix),
        is_acc_raw = grepl("raw", acc_col),
        is_acc_eobs = grepl("eobs", acc_col)
      ) |> 
      dplyr::select(!all_of(c(acc_col, axes_col)))
    
    
  }else if(acc_format == "plain"){
    
    # determine if acc is raw and from eobs tags
    acc_axis_cols <- grep("acceleration_(raw_)?[xXyYzZ]", cols, value = TRUE) 
    is_acc_raw <- grepl("raw", acc_axis_cols[1])
    is_acc_eobs <- grepl("eobs", acc_axis_cols[1])
    
    # define standardized names for axis columns in output
    axes_names <- paste0("acc", stringr::str_sub(acc_axis_cols, start = -2, end = -1))

    # Identify acc bursts based on timestamp and nest bursts as list-column
    #
    # NOTE: Some accelerometer tags provide tilt data alongside acceleration
    # samples, which here also gets nested
    #
    # HACK (minor): For some unclear reason, nesting does not work unless data
    # is converted to data.frame. Therefore, need to collect required
    # information for back-conversion to move2
    
    # reshape data
    .data <- .data |> 
      # standardize acc axis column names
      dplyr::rename_with(
        .fn = \(x) axes_names, #~paste0("acc", stringr::str_sub(.x, start = -2, end = -1)), 
        .cols = dplyr::matches("acceleration_(raw_)?[xXyYzZ]")
      ) |>
      as.data.frame() |>
      # identify data bursts
      dplyr::mutate(burst_id = mark_time_bursts(.data[[tm_col]])) |> 
      # nest acc data by bursts (including tilt, if present, event_id and time as they are row-specific)
      tidyr::nest(acc_bursts = c(dplyr::matches("_[xXyYzZ]\\b"), event_id, dplyr::all_of(tm_col))) |> 
      dplyr::mutate(
        # set time column as the starting time of burst
        timestamp = purrr::map(acc_bursts, ~ first(.[[tm_col]])),  
        # drop non-acceleration columns and convert to matrix for faster summarising below
        acc_bursts = purrr::map(acc_bursts, ~ as.matrix(.[, axes_names])), 
        # add extra info on type of acc data
        is_acc_raw, is_acc_eobs 
      ) |>
      tidyr::unnest(timestamp)
    
    # update time column name
    tm_col <- "timestamp"
    
  }else{
    stop("Unable to determine the format of the ACC data.")
  }
  
  # Summarise and add relevant attributes  ----------------------------
  .data <- .data |>
    dplyr::mutate(
      mean = purrr::map(acc_bursts, ~ apply(., 2, mean, na.rm = TRUE)),
      var = purrr::map(acc_bursts, ~ apply(., 2, var, na.rm = TRUE))
    ) |>
    tidyr::unnest_wider(c(mean,var), names_sep = "_") |>
    # add variables required for binding to location data
    dplyr::mutate(
      yearmonthday = gsub( "-", "", substr(timestamp, 1, 10)),
      hourmin = time_to_decimal_hours(timestamp)
    )
  
  # convert back to move2 ----------------------------
  .data <- .data |>
    mt_as_move2(time_column = tm_col, track_id_column = id_col) |>
    mt_set_track_data(track_dt)
  
  return(.data)
}



#' /////////////////////////////////////////////////////////////////////////////
#' Classify format of accelerometer data
#' 
#' Three possible outcomes:
#' - "nested" type: one burst of ACC samples per row
#' - "plain" type: ACC samples from one burst provided as one measurement per row 
#' (for each active axis)
#' - "undetermined": unable to classify due to inconsistencies in columns format
#'
#' **Important**: Function to be applied at individual level. Otherwise move2
#' binds datasets and thus columns from both formats are used together, making
#' the function obsolete
#' 
#' @param .data a `move2` object with accelerometer data
#' 
get_acc_format <- function(.data){
  
  # input validation ------
  if(!mt_is_move2(.data)){
    stop("Called `.data` must be a `move2` object")
  }
  
  # get column names -------
  cols <- names(.data)
  
  # default classification
  out <- "undetermined"
  
  # Stage 1 --------------
  # Detect presence of column with bursts of ACC sensor values nested per row,
  # based on expected names as described in Movabanks Attribute Dictionary
  if(any(grepl("(A|a)ccelerations(_raw)?\\b", cols))){
    # extra check to validate nested format: acc axis are defined in column
    # `acceleration_axes` by design
    acc_axes_idx <- grep("acceleration_axes", cols)
    if(any(grepl("[xXyYzZ]", .data[[acc_axes_idx[1]]]))){
      out <- "nested"
    } 
  }
  
  # Stage 2 ----------------
  # Detect presence of columns named with "acceleration_" (singular),
  # suggesting non-nested format i.e. one acc sensor value per row
  acc_col_idx <- grep("acceleration_(raw_)?[xXyYzZ]", cols)
  
  if(length(acc_col_idx) != 0){
    # update classification if column has any non-NA element
    if(!all(is.na(.data[[acc_col_idx[1]]]))){
      out <- "plain"  
    }
  }
  return(out)
}


#' /////////////////////////////////////////////////////////////////////////////
#' Detects time bursts in a vector of timestamps
#' 
#' Identifies bursts of time within a vector of timestamps based on the lag
#' between consecutive values and a maximum burst period (`max_period`).
#' Consecutive timestamps lagged by more than `max_period` are regarded as
#' belonging to separate bursts.
#'
#' @param timestamp a vector of class "POSIXct" with timestamps
#' @param max_period numeric scalar, the maximum accepted period of bursts (i.e.
#'   time lag between consecutive values in a burst).
#' 
#' @details 
#' Burst period: duration between consecutive values in a burst. Unit: secs
#' Burst frequency: number of values per second in a burst. Unit: Hz (e.g.
#' samples/sec)
#' 
#' @returns a numeric vector of the same length as `timestamp` providing the id
#'   for each detected time burst
mark_time_bursts <- function(timestamp, max_period = 1){
  
  if(!is.POSIXct(timestamp)) stop("`timestamp` must be of class POSIXct")

  lags <- units::as_units(diff(timestamp))
  # making sure lags are in secs
  units(lags) <- units::make_units(s)
  
  lags <- c(as.numeric(lags), NA)
  nlags <- length(lags)
  burst_id <- rep(NA, nlags)
  id <- 1
  burst_id[1] <- id
  
  if(all(lags > max_period, na.rm = TRUE)){
    burst_id[2:nlags] <- 2:nlags
  }else{
    for(i in 2:(nlags-1)){
      if(i == (nlags-1)){
        burst_id[c(i, i+1)] <- id  
      }else{
        lag_diff <- lags[i] - lags[i-1]
        # TODO: deal with NAs in lags due to missing values in `timestamp`
        if(lag_diff > max_period){
          burst_id[i] <- id
          id <- id + 1
        }else{
          burst_id[i] <- id  
        }
      }
    }
  }
  burst_id
}



#' /////////////////////////////////////////////////////////////////////////////
#'  Turn string of accelerometer data into a matrix
#' 
#' Converts burst of acc data provided as a single character string into a
#' matrix, assuming values are single-space separated.
#' 
#' @param burst_str a character string
#' @param acc_axis a character string
acc_string_to_matrix <- function(burst_str, acc_axis){
  
  # input validation
  if(!is.character(burst_str)) stop("`burst_str` must be a character string")
  if(length(burst_str)>1) stop("`burst_str` must be a character string of length 1")
  if(!is.character(acc_axis) & !is.factor(acc_axis)) stop("`acc_axis` must be a character")
  if(length(acc_axis)>1) stop("`acc_axis` must be a character string of length 1")
  
  # split sampled acceleration values along activated axes (X and/or Y and/or Z)
  burst_str <- as.numeric(stringr::str_split(burst_str, "\\s")[[1]])
  # get the ACC axes enabled in the current entry
  axesId <- tolower(sort(stringr::str_split(acc_axis, "")[[1]]))
  # matrix with sampled values allocated to the appropriate activated axes ("Measurements
  # alternate one measurement for each active axis in alphabetical order")
  matrix(
    burst_str, 
    byrow = TRUE, ncol = length(axesId), 
    dimnames = list(NULL, paste0("acc_", axesId))
  )
}


#' /////////////////////////////////////////////////////////////////////////////
#' Extracts the time of the day in decimal hours from a timestamp
#' 
time_to_decimal_hours <- function(datetime){
  
  if(!is.POSIXct(datetime)) stop("Provided object is not of type POSIXct")
  
  hour = lubridate::hour(datetime)
  minute = lubridate::minute(datetime)
  secs = lubridate::second(datetime)
  hour + minute/60 + secs/3600
}
