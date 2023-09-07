### -----------------------------------------------------------------------------------------------------------
#' Prepares and returns new ACC data from latest API query to ACC EOBS vultures
#' 
accMasseur_eobsVult_updated <- function(acc_dwnldDt){
  
  #' ~~ Prepare ACC data
  #' Note: From October 2020, a new set of tags with were added to the study. Acceleration raw data for these 
  #' tags is in different columns (acceleration_raw_x, acceleration_raw_y, acceleration_raw_z) and are provided 
  #' in a different format (one value per row entry, as opposed to multiple values nested per row entry). Thus each type of 
  #' data requires different processing 
  #' 
  #' Per-row Type
  #'   1. slice the data for a given period of time and summarise accordingly
  #'   2. Add columns with relevant time-related variables
  #' 
  #' Nested Type:
  #'   1. summarise the sampled raw values from each accelerometer axis for each row entry
  #'   2. Add columns with relevant time-related variables
  
  
  # Subset for nested (older) type
  acc_dwnldDt_nested <- acc_dwnldDt %>%
    filter(eobs_acceleration_axes != "")
  
  # Subset for per row (newer) type
  acc_dwnldDt_perRow <- acc_dwnldDt %>%
    filter(eobs_acceleration_axes == "")
  
  # ------------------------- #
  #    Process nested type    #
  # ------------------------- #
  
  #options(future.globals.maxSize = 1000 * 1024 ^ 2)
  
  if(nrow(acc_dwnldDt_nested) > 0){
    
    plan(multisession)
    
    acc_new_nested <- acc_dwnldDt_nested %>%
      future_pmap_dfr(function(eobs_acceleration_axes, eobs_accelerations_raw, ...){
        #pmap_dfr(function(eobs_acceleration_axes, eobs_accelerations_raw, ...){
        
        # get the sampled raw values of acceleration along "activated" axes (X and/or Y and/or Z)
        acc_axes <- as.numeric(str_split(eobs_accelerations_raw, " ")[[1]])
        
        # get the ACC axes enabled in the current entry
        axesId <- str_split(eobs_acceleration_axes, "")[[1]]
        
        # allocate sampled raw values to the corresponding enabled axes
        # ("Measurements alternate one measurement for each active axis in alphabetical order")
        acc <- as_tibble(matrix(acc_axes, byrow = TRUE, ncol = length(axesId), 
                                dimnames = list(NULL, axesId)))
        
        # calculate means and variances of sampled values in each axis
        acc_summaries <- summarise_all(acc, .funs = list(mean, var))
        colnames(acc_summaries) <- tolower(c(paste0("mean", axesId), paste0("var", axesId)))
        
        # little trick to impose consistency in outputs, regardless of which axes are enabled in the current entry
        dummy_tbl <- tibble(meanx = numeric(0), meany = numeric(0), meanz = numeric(0), varx = numeric(0), vary = numeric(0), varz = numeric(0))
        acc_summaries %<>% bind_rows(dummy_tbl, .)
        
        return(acc_summaries)
      }, .progress = TRUE) %>%
      bind_cols(acc_dwnldDt_nested, .) %>%
      mutate(hour = hour(eobs_start_timestamp),
             minute = minute(eobs_start_timestamp), 
             secs = second(eobs_start_timestamp),
             hourmin = hour + minute/60 + secs/3600, 
             ODBA = sqrt(meanx**2+meany**2+meanz**2),  # Not sure what this is
             yearday = yday(eobs_start_timestamp),
             month = month(eobs_start_timestamp),
             year = year(eobs_start_timestamp),
             day = day(eobs_start_timestamp),
             yearmonthday = str_replace_all(str_sub(eobs_start_timestamp, 1, 10), "-", "") #,
             #tag = as.character(mt_track_id(.))
             ) %>%
      mutate_if(is.factor, as.character)
    
    plan(sequential)
    
    #' ~~ only keep entries with acc data for the 3 axes
    acc_new_nested %<>% filter(eobs_acceleration_axes == "XYZ")
    
    #think this is fking things up
    #' ~~ Drop (apparently) unnecessary columns, including the raw sampled accelerometer values
    acc_new_nested %<>%
      dplyr::select(-c(contains("marked_outlier"), starts_with("acceleration_raw_"), data_decoding_software, contains("acceleration"), contains("orientation"),
                contains("magnetic"), event_id,
                eobs_key_bin_checksum, eobs_accelerations_raw, 
                timestamp))
  }else{
    acc_new_nested <- tibble()
  }
  
  
  # ------------------------- #
  #    Process per row type   #
  # ------------------------- #
  
  # summarizing ACC axes values per 1sec periods, which approximates to what's done on nested data
  
  # browser()
  
  if(nrow(acc_dwnldDt_perRow)>0){
    
    acc_new_perRow <- acc_dwnldDt_perRow %>%
      mutate(timestamp_minFloor = floor_date(timestamp, "min")) %>%
      group_by(individual_id, mt_track_id(.), #deployment_id, 
               timestamp_minFloor) %>%
      summarise(
        #eobs_start_timestamp = first(timestamp),
        #update_ts = first(update_ts),
        #sensor_type_id = first(sensor_type_id),
        #event_id = first(event_id),
        #tag_local_identifier = first(tag_local_identifier),
        #local_identifier = first(local_identifier),
        #study_name = first(study_name),
        #dwnld_timestamp = first(dwnld_timestamp),
        #meanx = mean(acceleration_raw_x, na.rm = TRUE),
        #meany = mean(acceleration_raw_y, na.rm = TRUE),
        #meanz = mean(acceleration_raw_z, na.rm = TRUE),
        #varx = var(acceleration_raw_x, na.rm = TRUE),
        #vary = var(acceleration_raw_y, na.rm = TRUE),
        #varz = var(acceleration_raw_z, na.rm = TRUE)
        meanx=0,
        meany=0,
        meanz=0,
        varx=0,
        vary=0,
        varz=0
      )  %>%
      ungroup() %>%
      mutate(hour = hour(eobs_start_timestamp),
             minute = minute(eobs_start_timestamp), 
             secs = second(eobs_start_timestamp),
             hourmin = hour + minute/60 + secs/3600, 
             ODBA = sqrt(meanx**2+meany**2+meanz**2),  # Not sure what this is
             yearday = yday(eobs_start_timestamp),
             month = month(eobs_start_timestamp),
             year = year(eobs_start_timestamp),
             day = day(eobs_start_timestamp),
             yearmonthday = str_replace_all(str_sub(eobs_start_timestamp, 1, 10), "-", "")#,
             #tag = as.character(mt_track_id(.))
             ) %>%
      mutate_if(is.factor, as.character) %>%
      dplyr::select(-timestamp_minFloor)
    
  }else{
    acc_new_perRow <- tibble()
  }
  

  #acc_new <- bind_rows(acc_new_nested, acc_new_perRow)
  

  
  if(nrow(acc_dwnldDt_perRow) > 0 & nrow(acc_dwnldDt_nested) > 0) {
    acc_new <- mt_stack(acc_new_nested, acc_new_perRow) # test
  } else {
    
    if(nrow(acc_dwnldDt_nested) > 0 & nrow(acc_dwnldDt_perRow) == 0) {
      acc_new <- acc_new_nested
    }
    
    if(nrow(acc_dwnldDt_nested) == 0 & nrow(acc_dwnldDt_perRow) > 0) {
      acc_new <- acc_new_perRow
    }
    
  }
  
  
  return(acc_new)
  
}


