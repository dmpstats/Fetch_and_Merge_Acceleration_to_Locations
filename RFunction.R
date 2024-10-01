library('move2')
library('lubridate')
library("units")
library("dplyr")
library("tidyr")
library("purrr")
library("stringr")
library("assertthat")
library("sf")
library("rlang")
library("keyring")

rFunction = function(data, 
                     usr, pwd, 
                     merging_rule = c("nearest", "latest"), 
                     store_acc_track_info = FALSE,
                     acc_timefilter = 0){
  
  # Input validation --------------------------------------------------------
  
  merging_rule <- rlang::arg_match(merging_rule)
  
  # The following assertions are a bit redundant given definitions in appspec.json and
  # checks done at the GUI level. HOWEVER, they're useful for local use and testing
  assertthat::assert_that(mt_is_move2(data))
  assertthat::assert_that(assertthat::is.string(usr))
  assertthat::assert_that(assertthat::is.string(pwd))
  assertthat::assert_that(is.logical(store_acc_track_info))
  assertthat::assert_that(assertthat::is.number(acc_timefilter))
  assertthat::assert_that(
    dplyr::between(acc_timefilter, 0, 30),
    msg = paste0(
      "The specified time interval of ", acc_timefilter, " mins (`acc_timefilter`)",
      " is outside the accepted range of values to filter Accelerometer data.",
      " Please provide a value between 0 and 30.")
  )
    
  
  # Set movebank login credentials ---------------------------------------------
  options("keyring_backend"="env")
  movebank_store_credentials(username = usr, password = pwd)

  
  # Collect input data details -----------------------------------------------
  logger.info("Collecting details about input data")

  trk_dt <- mt_track_data(data)
  trk_id_col <- mt_track_id_column(data)
  trk_dt_colnames <- names(trk_dt)
  study_id_col <- grep("study(_|.)id", trk_dt_colnames, value = TRUE)
  study_id <- unique(trk_dt[[study_id_col]])
  ind_id_col <- grep("individual(_|.)id", trk_dt_colnames, value = TRUE)
  sens_id_col <- grep("^sensor(_|.)type(_|.)ids$", trk_dt_colnames, value = TRUE)
  dwnld_perm_id_col <- grep("i(_|.)have(_|.)download(_|.)access", 
                            trk_dt_colnames, value = TRUE)
  time_id_col <- mt_time_column(data)
  event_id_col <- grep("event(_|.)id", trk_dt_colnames, value = TRUE)
  
  
 
  # Input data prep ------------------------------------------------------------
  data <- data |> 
    # set event and track datasets underlying move2 object as tibble (tidier printing)
    mt_as_move2_tibble() |> 
    # drop events with missing timestamps
    dplyr::filter(!is.na(.data[[time_id_col]])) |> 
    # order by time within track
    dplyr::arrange(.data[[trk_id_col]], .data[[time_id_col]])
    
  
  
  # AAC availability checks ---------------------------------------------------
  logger.info("Checking ACC data availability")
  
  ## Retrieve sensor info, if absent in data
  if(length(sens_id_col) == 0){
    
    logger.warn("Sensor information is missing in the input data. Fetching required data from Movebank")
    
    # write(paste0("Pinging issue with missing sensor info ", now()), file = "error_ping.txt", append = TRUE)
    
    sensor_info <- trk_dt |> 
      dplyr::group_split(.data[[study_id_col]]) |> 
      purrr::map(\(x){
        movebank_download_deployment(
          study_id = unique(x[[study_id_col]]), 
          individual_id = x[[ind_id_col]]
        )}
      ) |> purrr::list_rbind() 
    
    logger.info(paste0("  |- Sensor data retrieved with success"))
    
    sens_id_col <- grep("^sensor(_|.)type(_|.)ids$", names(sensor_info), value = TRUE)
    deploy_id_col <- grep("^deployment_|.)id", names(sensor_info), value = TRUE)
    
    sensor_info <- sensor_info |> 
      dplyr::select(all_of(c(study_id_col, ind_id_col, deploy_id_col, sens_id_col)))
    
    trk_dt <- trk_dt |> 
      left_join(sensor_info, by = c(study_id_col, ind_id_col, deploy_id_col))
  } 
  
  
  ## Fetch download permissions if info absent in input data
  if(length(dwnld_perm_id_col) == 0){  
    # Here assuming multiple studies in input data, which currently won't happen
    # in MoveApps (only one study allowed per workflow). Doesn't impact
    # performance so keeping it for the moment
    dwnld_perm <- sapply(
      study_id, 
      \(x) movebank_download_study_info(study_id = x)$i_have_download_access
    )
    dwnld_perm_id_col <- "i_have_download_access"
    trk_dt[[dwnld_perm_id_col]] <- rep(dwnld_perm, each = rle(trk_dt[[study_id_col]])$lengths)
  }
  
  
  
  ## Is ACC data collected? 
  trk_dt$is_acc_collected <- grepl("acceleration", trk_dt[[sens_id_col]], 
                                   ignore.case = TRUE)

  # Escape if no ACC for any of the animals
  if(all(!trk_dt$is_acc_collected)){
    logger.warn(
      paste("Accelerometer data is not collected for any of the animals",
            "in the input data set. Skipping ACC dowloading and merging steps.")
    )
    # COMBAK: testing if the following warning message appears in the APP's output panel
    rlang::warn(
      paste("Accelerometer data is not collected for any of the animals in the ",
            "input data set. Skipping ACC dowloading and merging steps."), 
            class = "warn_acc_not_collected")

    data_output <- data |>  mt_set_track_data(trk_dt) |>  mutate(acc_dt = list(NULL))
    
    logger.info("Done! App has finished all its tasks.")
    return(data_output)
  }
  
  # Escape if no permission for any of the animals
  if(all(!trk_dt[[dwnld_perm_id_col]])){
    logger.warn(
      paste("User does not have permission to download Accelerometer data for any of",
            "the animals in the input data set. Skipping ACC dowloading and merging steps.")
    )
    
    # COMBAK: testing if the following warning message appears in the APP's output panel
    rlang::warn(
      paste("User does not have permission to download Accelerometer data for any of",
            "the animals in the input data set. Skipping ACC dowloading and merging steps."),
            class = "warn_no_download_permission")
    
    
    data_output <- data |> mt_set_track_data(trk_dt) |> mutate(acc_dt = list(NULL))
    
    logger.info("Done! App has finished all its tasks.")
    return(data_output)
  }
  

  
  
  # Gather info to set up ACC download   ---------------------------------------
  acc_dwnld_info <- trk_dt |> 
    dplyr::select(
      dplyr::all_of(c(study_id_col, ind_id_col, trk_id_col, sens_id_col)), 
      is_acc_collected, i_have_download_access
    )

  ## Add timestamps of first event and final event of each animal in input data
  dwnld_timespan <- data |> 
    sf::st_drop_geometry() |> 
    dplyr::group_by(.data[[trk_id_col]]) |> 
    dplyr::summarise(
      acc_dwn_start_time = min(.data[[time_id_col]], na.rm = TRUE), 
      acc_dwn_end_time = max(.data[[time_id_col]], na.rm = TRUE)
    ) 
  
  acc_dwnld_info <- dplyr::left_join(acc_dwnld_info, dwnld_timespan, by = trk_id_col)
  
  
  
  # Download ACC data ----------------------------------------------------------
  logger.info("Downloading ACC data for each track")
  
  ## Iterate over rows and nest downloaded data. 
  acc <- acc_dwnld_info |> 
    dplyr::as_tibble() |> 
    dplyr::mutate(
      acc_dt = purrr::pmap(
        .l = list(.data[[study_id_col]], .data[[ind_id_col]], 
                  is_acc_collected, acc_dwn_start_time, acc_dwn_end_time), 
        .f = \(std, ind, acc_collected, start, end){
          if(acc_collected){
                movebank_download_study(
                  study_id = std,
                  sensor_type_id = "acceleration",
                  individual_id = ind,
                  timestamp_start = start,
                  timestamp_end = end
                ),
                # move2_error_movebank_api_license_not_accepted = function(cnd){
                #   warning("ACC download failed because user has not accepted api license terms of the study")
                #   NULL
                # }
              ),
              warning = function(cnd){
                cnd_msg <- conditionMessage(cnd)
                cnd_exp <- "no non-missing arguments to m(in|ax); returning -?Inf"
                if(grepl(cnd_exp, cnd_msg)) rlang::cnd_muffle(cnd)
              })
          } else NULL
        })
    )
  
  ## Add details on downloaded data
  acc <- acc |>
    dplyr::mutate(
      acc_in_timespan = purrr::map_lgl(acc_dt, not_null), 
      .after = acc_dwn_end_time)

  ## print summary to log 
  acc_dwnld_summary <- acc |> 
    mutate(acc_nrows = purrr::map2_int(acc_in_timespan, acc_dt, ~ifelse(.x, nrow(.y), 0))) |> 
    dplyr::select(-acc_dt, -dplyr::all_of(ind_id_col))
  
  logger.info(
    paste0(
      "\n\n====== Summary of downloaded ACC data =======\n\n",
      paste0(capture.output(print(acc_dwnld_summary, n = Inf)), collapse = "\n"),
      "\n"
    ))  
  
  
  
  # Process ACC data ----------------------------------------------------------
  logger.info("Processing downloaded Accelerometer data")
  
  ## Process by iterating row-wise (i.e. per animal) and overwrite nested
  ## unprocessed ACC data
  acc <- acc |> 
    dplyr::mutate(
      acc_dt = purrr::map(acc_dt, \(x){
        if(not_null(x)){
          process_acc(x, acc_timefilter)
        } else NULL
      }, 
      .progress = TRUE)
    )
  
  # Remind user on thinning of downloaded ACC using time filter
  if(acc_timefilter > 0){
    logger.info(
      paste0("Downloaded ACC data thinned to a time interval of ", acc_timefilter, " mins")
    )
  }
  
  
  
  # Merge ACC data to input location data --------------------------------------
  logger.info("Merging ACC data to location data")
  
  # TODO??? each record of location data must be unique
  # data <- mt_filter_unique(data, criterion = "first")
  
  ## Separate acc event data from acc track data AND set event data as tibble.
  ## Two reasons:
  ## (i) efficiency: much faster indexing on tibble than move2
  ## (ii) reduce redundancy of repeated track data on each subset of accdata
  ## after the binding step
  acc <- acc |> 
    dplyr::mutate(
      acc_trk_dt = purrr::map(acc_dt, \(x) if(is.null(x)) NULL else mt_track_data(x)),
      acc_dt = purrr::map(acc_dt, \(x) if(is.null(x)) NULL else as_tibble(x))
    )

  ## Join acc data and location data (animal-level)
  all_data <- acc |> 
    dplyr::mutate(
      loc_dt = purrr::map(
        .data[[trk_id_col]], 
        ~filter_track_data(data, .track_id = .))
      )
     
  ## Merge acc data to loc data based on time according to merging rule
  ## (row-wise processing, i.e at animal-level)
  all_data <- all_data |> 
    dplyr::mutate(
      loc_dt = purrr::pmap(
        .l = list(acc_dt, loc_dt), 
        .f = \(a, l){
          merge_acc_to_loc(a, l, merging_rule)
        }, 
        .progress = TRUE)
    )
  
  
  
  # Wrap-up and output   -----------------------------------------------
  logger.info("Preparing data for output")
  
  # Stack up animal-level updated location data for output
  if(nrow(all_data) == 1){
    # required because, for some unidentified reason, `mt_stack()` fails for single element lists in some studies
    data_output <- all_data$loc_dt[[1]]
  }else{
    data_output <- mt_stack(all_data$loc_dt)
  }
  
  # Add acc download info to track data
  updated_trk_dt <- dplyr::left_join(
    mt_track_data(data_output),
    all_data |> dplyr::select(-acc_dt, -acc_trk_dt, -loc_dt)
  )
  
  data_output <- data_output |> mt_set_track_data(updated_trk_dt)
  
  # Add chosen merging rule as an object attribute
  attr(data_output, "acc_merging_rule") <- merging_rule
  
  if(store_acc_track_info){
    # Add original track data from downloaded acc data as an attribute of the output loc data
    acc_trk_dt <- all_data$acc_trk_dt |> purrr::compact()
    
    if(length(acc_trk_dt) > 0){
      if(length(acc_trk_dt) == 1){
        acc_trk_dt <- acc_trk_dt[[1]]
      }else if(length(acc_trk_dt) > 1){
        acc_trk_dt <- acc_trk_dt |> purrr::list_rbind()
      }  
      attr(data_output, "acc_track_data") <- acc_trk_dt
    }
  }
  
  
  # Export relevant data as artefacts ------------------------------------------
  saveRDS(acc, file = appArtifactPath("downloaded_acc_data.rds"))
  
  
  # EOF ------------------------------------------------------------------------
  logger.info("Done! App has finished all its tasks.")
  
  return(data_output)
}






#' /////////////////////////////////////////////////////////////////////////////
merge_acc_to_loc <- function(.acc, .loc, merging_rule){
  
  if(not_null(.acc)){
    
    # ensuring correct time cols are called below
    acc_time_col <- mt_time_column(.acc)
    loc_time_col <- mt_time_column(.loc)
    
    # indexes mapping timepoints in acc data to timeline of loc data
    time_mapping <- snap_times_to_timeline(
      timeline = .loc[[loc_time_col]], 
      timepoints = .acc[[acc_time_col]], 
      rule = merging_rule
    )
    
    # split acc data to list following time mapping
    acc_ls <- split(time_mapping, time_mapping$tmln_idx) |> 
      map(~.acc[.x$tmpt_idx, ])
    
    # merge acc data to each event in location data. Need to populate
    # list column first so that indexing works
    .loc$acc_dt <- list(NULL)
    .loc$acc_dt[na.omit(unique(time_mapping$tmln_idx))] <- acc_ls   # na.omit to deal with missing timestamps (if any)
    return(.loc)
    
  }else{
    dplyr::mutate(.loc, acc_dt = list(NULL))
  }
}




#' /////////////////////////////////////////////////////////////////////////////
#' Allocates time-points to a reference timeline based on a merging `rule`:
#'  
#'  @param timeline a date-time vector, providing the reference timeline
#'  @param timpoints a date-time vector, the time-points of interest
#'  @param rule a single character string, taking one of the following two values:
#'    - `nearest`: allocates each time-point to the closest timeline element, 
#'    which can either occur before or after the time-point
#'    - `latest`: allocates each time-point to the latest timeline element occurring 
#'    before the time-point
#' 
#' Taking a more efficient (speed-wise and memory-wise)  approach by binding timeline and
#' timepoints and then sorting and indexing to apply the merging rules. Previous
#' approach of computing the difference matrix was very expensive in memory
#' allocation and, on far-sight, way too crude when dealing with inputs covering
#' weeks of data

snap_times_to_timeline <- function(timeline, 
                                   timepoints, 
                                   rule = c("nearest", "latest")){
  
  # input validation
  if(is.unsorted(timeline, na.rm = TRUE)) stop("`timeline` must be ordered")
  
  match.arg(rule)
  
  if(min(timepoints, na.rm = TRUE) < min(timeline, na.rm = TRUE) 
     | max(timepoints, na.rm = TRUE) > max(timeline, na.rm = TRUE)){
    stop("timepoints must be contained between range(timeline)")
  }
  
  # timeline matrix, with flag for timeline
  tmln_mtx <- matrix(
    c(rep(1L, length(timeline)), 1L:length(timeline), timeline), 
    ncol = 3, 
    byrow = FALSE, 
    dimnames = list(NULL, c("is.tl", "idx", "t")))
  
  # timepoint matrix, with flag for timeline
  tpts_mtx <- matrix(
    c(rep(0L, length(timepoints)), 1L:length(timepoints), timepoints), 
    ncol = 3, byrow = FALSE, dimnames = list(NULL, c("is.tl", "idx", "t")))
  
  # bind matrices
  t_mtx <- rbind(tmln_mtx, tpts_mtx)
  # sort by time and timeline flag
  t_mtx <- t_mtx[order(t_mtx[, "t"], -t_mtx[, "is.tl"]), ]
  
  # add end row to absorb last tp value when tp == tl
  t_mtx <- rbind(t_mtx, c(1, nrow(tmln_mtx)+1, max(tmln_mtx[, "t"])))
  
  # create sub-table with indices for consecutive timeline pairings
  tmln_idx <- which(t_mtx[, "is.tl"] == 1)
  tmln_idx_mtx <- matrix(c(tmln_idx, lead(tmln_idx)), ncol = 2, byrow = FALSE)
  tmln_idx_mtx <- tmln_idx_mtx[-nrow(tmln_idx_mtx), ] # drop last row as its not relevant
  
  # iterate by row, i.e. over indices of each pair of consecutive timelines
  out_list <- apply(tmln_idx_mtx, 1, \(x){
    
    consecutive_idxs <- (x[[2]] - x[[1]]) == 1
    
    if(!consecutive_idxs){
      
      if(rule == "latest"){
        
        tp_idx <- t_mtx[(x[[1]]+1):(x[[2]]-1), "idx"]
        
        tl_idx <- rep(t_mtx[x[[1]], "idx"], length(tp_idx))
        
      }else if(rule == "nearest"){
        
        tp_within <- t_mtx[(x[[1]]+1):(x[[2]]-1), c("idx", "t"), drop=FALSE]
        
        midpt <- (t_mtx[x[[1]], "t"] + t_mtx[x[[2]], "t"])/2
        
        below_midpt <- tp_within[, "t"] <= midpt
        
        tp_idx <- tp_within[, "idx"]
        
        tl_idx <- ifelse(below_midpt, t_mtx[x[[1]], "idx"], t_mtx[x[[2]], "idx"])
        
      }
      matrix(c(tp_idx, tl_idx), ncol = 2, byrow = FALSE)
    }
    
  }, simplify = FALSE)
  
  # bind matrices
  out <- do.call(rbind, out_list)
  colnames(out) <- c("tmpt_idx", "tmln_idx")
  
  # test that nrow of indices is the same as length of input timepoints, give 
  # that there are no NAs and timepoints are whithin timeline's timespan
  if(nrow(out) != length(timepoints)){
    stop("Something went wrong! Please get in touch with App maintainers")
  }
  
  # convert tibble to allow for easier colname subsetting
  as_tibble(out) |> 
    mutate(
      tmpt_idx = as.integer(tmpt_idx),
      tmln_idx = as.integer(tmln_idx)
    )
}


#' /////////////////////////////////////////////////////////////////////////////
#' ACC data processing
#' 
#' Processes the ACC data at an individual level into a standardized ACC format.
#' The output object contains one burst of ACC measurements per row (nested in
#' list-column named `acc_burst`). Each row specifies an event when a burst of
#' ACC values (per active axis) were sampled, with `tm_col` providing the start
#' time of the burst. Data can optionally be thinned to only return a single
#' event whithin a given `time_filter` time window (in minutes)
#' 
#' @param data a `move2` object with accelerometer data
#' @param time_filter a numeric value specifying the time interval to subset the
#'   data records (`0` means no thinning applied). Unit: minutes
#'   
process_acc <- function(data, time_filter = 0){
  
  # input validation -------------------------------------
  # check if time is in ascending order
  assertthat::assert_that(mt_is_time_ordered(data))
  
  # check if data belongs to a single individual
  if(length(unique(mt_track_id(data))) > 1){
    stop("Unable to pre-process ACC data - provided data must be from a single individual")
  } 
  
  # store move2 data and identifiers, to retrieve later ------------------
  track_dt <- mt_track_data(data)
  tm_col <- mt_time_column(data)
  id_col <- mt_track_id_column(data)
  event_id_col <- grep("event(_|.)id", names(data), value = TRUE)
  
  # Pre-processing  ----------------------------------------------------
  # For extra safety, as downloaded data should come already ordered and without
  # NAs in timestamps
  data <- data |>
    # drop events with missing timestamps
    dplyr::filter(!is.na(.data[[tm_col]])) |>
    # order by time within track
    dplyr::arrange(.data[[id_col]], .data[[tm_col]])

  
  # Standardize format -------------------------------------------------
  
  # Depending on format type (i.e. nested or plain), provided data reshaped
  # accordingly to have bursts of ACC nested in a list-column named
  # `acc_bursts`, i.e. one burst per row. Type (raw or non-raw) and origin (eobs
  # or non-eobs) of ACC also recorded.
  #
  # NOTE: Code assumes type of data and tag origin are mutually exclusive at the
  # animal level - i.e., data can only contain either raw or non-raw acc, from
  # eobs or non-eobs tags
  
  # get column names
  cols <- names(data)
  
  # get format of acc data
  acc_format <- get_acc_format(data)
  
  if(acc_format == "nested"){
    
    # thin data by time window. Doing it here for efficiency (i.e. only
    # processing bursts for the thinned data)
    if(time_filter > 0){
      data <- data |> 
        mt_filter_per_interval(criterion = "first", unit = paste0(time_filter, " mins"))  
    }
    
    # get column names with nested acc and axes specification
    acc_col <- grep("(A|a)ccelerations(_raw)?$", cols, value = TRUE)
    axes_col <- grep("acceleration_axes", cols, value = TRUE)
    
    # convert character-type acc data to matrix and store in list-column
    data <- data |> 
      dplyr::mutate(
        acc_burst = purrr::map2(.data[[acc_col]], .data[[axes_col]], acc_string_to_matrix),
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

    # Identify acc bursts based on timestamp and nests bursts as list-column
    # NOTE: Some accelerometer tags provide tilt data alongside acceleration
    data <- data |> 
      # tidyr::nest() doesn't work with move2, so needs convertion to tibble
      dplyr::as_tibble() |>
      # standardize acc axis column names
      dplyr::rename_with(
        .fn = \(x) axes_names,
        .cols = dplyr::matches("acceleration_(raw_)?[xXyYzZ]$")
      ) |>
      # identify data bursts
      dplyr::mutate(burst_id = mark_time_bursts(.data[[tm_col]])) |> 
      # nest acc data by bursts (including tilt, if present, event_id and time, as they're all row-specific)
      tidyr::nest(
        acc_burst = c(dplyr::matches("_[xXyYzZ]$"), event_id, dplyr::all_of(tm_col))
        ) |> 
      dplyr::mutate(
        # set time column as the starting time of burst
        timestamp = purrr::map(acc_burst, ~ first(.[[tm_col]])),  
        # set even ID as the id of first entry of burst
        event_id = purrr::map(acc_burst, ~ first(.[[event_id_col]])),
        # Derive burst duration, in secs
        acc_burst_duration = purrr::map_dbl(acc_burst, \(x){
          difftime(last(x$timestamp), first(x$timestamp), units = "secs")}),
        # Derive acc sampling frequency
        acc_sampling_frequency = purrr::map_int(acc_burst, nrow)/acc_burst_duration,
        acc_sampling_frequency = units::set_units(acc_sampling_frequency, Hz),
        # drop non-acceleration columns and convert to matrix for consistency with nested format above
        acc_burst = purrr::map(acc_burst, ~ as.matrix(.[, axes_names])),
        # extra info on type of acc data
        is_acc_raw, is_acc_eobs
      ) |>
      tidyr::unnest(c(timestamp, event_id))
    
    # update time column name
    tm_col <- "timestamp"
    
    # convert back to move2 
    data <- data |>
      mt_as_move2(time_column = tm_col, track_id_column = id_col) |>
      mt_set_track_data(track_dt)
    
    # thin data by time window. For plain format, thinning only after reshaping,
    # otherwise filtering would be applied to single raw acc values
    if(time_filter > 0){
      data <- data |> 
        mt_filter_per_interval(criterion = "first", unit = paste0(time_filter, " mins"))
    }
    
  }else{
    stop("Unable to determine the format of the ACC data.")
  }

  return(data)
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
#' @param data a `move2` object with accelerometer data
#' 
get_acc_format <- function(data){
  
  # input validation ------
  assertthat::assert_that(mt_is_move2(data))
  
  # get column names -------
  cols <- names(data)
  
  # default classification
  out <- "undetermined"
  
  # Phase 1 --------------
  # Detect presence of column with bursts of ACC sensor values nested per row,
  # based on expected names as described in Movabanks Attribute Dictionary
  if(any(grepl("(A|a)ccelerations(_raw)?\\b", cols))){
    # extra check to validate nested format: acc axis are defined in column
    # `acceleration_axes` by design
    acc_axes_idx <- grep("acceleration_axes", cols)
    if(any(grepl("[xXyYzZ]", data[[acc_axes_idx[1]]]))){
      out <- "nested"
    } 
  }
  
  # Phase 2 ----------------
  # Detect presence of columns named with "acceleration_" (singular),
  # suggesting non-nested format i.e. one acc sensor value per row
  acc_col_idx <- grep("acceleration_(raw_)?[xXyYzZ]", cols)
  
  if(length(acc_col_idx) != 0){
    # update classification if column has any non-NA element
    if(!all(is.na(data[[acc_col_idx[1]]]))){
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
#' @param max_period numeric scalar, the maximum period in bursts (i.e.
#'   time lag between consecutive values in a burst). Unit: seconds
#' 
#' @details 
#' Burst period: duration between consecutive values in a burst. Unit: secs
#' Burst frequency: number of values per second in a burst. Unit: Hz (e.g.
#' samples/sec)
#' 
#' @returns a numeric vector of the same length as `timestamp` providing the id
#'   for each detected time burst
mark_time_bursts <- function(timestamp, max_period = 1){
  
  if(!lubridate::is.POSIXct(timestamp)) stop("`timestamp` must be of class POSIXct")

  lags <- units::as_units(diff(timestamp))
  
  # making sure lags and max_period are in secs
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
#' @param burst_str a character string comprising a sample of acc values
#'   collected during a burst. Values are separated by single space
#' @param acc_axis a character string specifying the ACC axis (i.e. "XYZ", "XY"
#'   or "X")
acc_string_to_matrix <- function(burst_str, acc_axis){
  
  # input validation
  assertthat::assert_that(assertthat::is.string(burst_str))
  if(!is.character(acc_axis) & !is.factor(acc_axis)) stop("`acc_axis` must be a factor or a character")
  if(length(acc_axis)>1) stop("`acc_axis` must be of length 1")
  
  # split sampled acceleration values along activated axes (X and/or Y and/or Z)
  burst_str <- as.numeric(stringr::str_split(burst_str, "\\s+")[[1]])
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
#' Add class `tbl_df` (aka 'tibble') to the event and track data sets in move2
#' objects, for tidier printing
#' 
#' NOTE: not sure if MoveApps IO settings  preserves the `tbl_df` class to
#' subsequent Apps, but it's worth the try...
mt_as_move2_tibble <- function(x){
  
  assertthat::assert_that(mt_is_move2(x))
  
  # return x unchanged if already of class tibble
  if(inherits(x, "tbl_df")){
    return(x) 
  }
  
  track_dt <- dplyr::as_tibble(mt_track_data(x))
  track_id_col <- mt_track_id_column(x)
  time_id_col <- mt_time_column(x)
  
  dplyr::as_tibble(x) |>
    mt_as_move2(time_column = time_id_col , track_id_column =  track_id_col) |> 
    mt_set_track_data(track_dt)
  
}


#' /////////////////////////////////////////////////////////////////////////////
#' Extracts the time of the day in decimal hours from a timestamp
#' 
time_to_decimal_hours <- function(datetime){
  
  if(!lubridate::is.POSIXct(datetime)) stop("Provided object is not of type POSIXct")
  
  hour = lubridate::hour(datetime)
  minute = lubridate::minute(datetime)
  secs = lubridate::second(datetime)
  hour + minute/60 + secs/3600
}


#' /////////////////////////////////////////////////////////////////////////////
#' Miscellaneous helpers
not_null <- Negate(is.null)

