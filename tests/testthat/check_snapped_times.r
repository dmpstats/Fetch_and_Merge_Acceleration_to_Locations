library(dplyr)
library(purrr)
library(tidyr)
library(move2)
library(rlang)

check_snapped_times <- function(timeline, timepoints, snapped_idx, rule = "latest"){
  
  snapped_times <- tibble(
    tmln = timeline[snapped_idx$tmln_idx],
    tmpts = timepoints[snapped_idx$tmpt_idx]
  )
  
  merged_times <- tibble(
    tmln = timeline,
    tmln_before = lag(tmln, default = min(tmln)-0.00001),
    tmln_after = lead(tmln, default = max(tmln)+0.00001),
  ) |> 
    arrange(tmln) |> 
    left_join(snapped_times, by = "tmln") |> 
    mutate(across(everything(), as.numeric)) |> 
    drop_na(tmpts) |> 
    nest(dt = tmpts) 
  
  
  if(rule == "latest"){
    
    merged_times |> 
      mutate(
        test = pmap_lgl(list(dt, tmln, tmln_after), \(pts, start, end){
          #browser()
          all(pts >= start & pts < end)
        }))
    
  }else if(rule == "nearest"){
    
    merged_times |> 
      mutate(
        test = pmap_lgl(list(dt, tmln, tmln_before, tmln_after), \(pts, alloc, before, after){
          #browser()
          
          alloc_dst <- abs(pts - alloc)
          before_dst <- abs(pts - before)
          after_dst <-abs(pts - after)
          all(alloc_dst <= before_dst & alloc_dst <= after_dst)
          
        }))
  }
}




# reciprocal function to check merged datasets (applicable to multiple animals)
check_merged_times <- function(merged_data, rule = "latest"){
  
  id_col <- mt_track_id_column(merged_data)
  tm_col <- mt_time_column(merged_data)
  
  merged_times <- merged_data |> 
    arrange(.data[[id_col]], .data[[tm_col]]) |> 
    mutate(
      loc_tm_before = lag(.data[[tm_col]], default = min(.data[[tm_col]])-0.00001),
      loc_tm_after = lead(.data[[tm_col]], default = max(.data[[tm_col]])+0.00001),
      acc_times = purrr::map(acc_dt, ~.$timestamp),
      by = id_col
    )
  
  
  if(rule == "latest"){
    
    merged_times |> 
      mutate(
        test = pmap_lgl(
          list(acc_times, .data[[tm_col]], loc_tm_after), 
          #list(acc_times, !!!syms(tm_col), loc_tm_after), 
          \(pts, start, end){
            #browser()
            if(!is.null(pts)){
              all(pts >= start & pts < end)  
            } else  NA
          }), 
        .by = id_col
      )
    
  }else if(rule == "nearest"){
    
    merged_times |> 
      mutate(
        test = pmap_lgl(
          list(acc_times, .data[[tm_col]], loc_tm_before, loc_tm_after), 
          #list(acc_times, !!!syms(tm_col), loc_tm_before, loc_tm_after),
          \(pts, alloc, before, after){
            #browser()
            if(!is.null(pts)){
              alloc_dst <- abs(pts - alloc)
              before_dst <- abs(pts - before)
              after_dst <-abs(pts - after)
              all(alloc_dst <= before_dst & alloc_dst <= after_dst)
            } else NA
            
          }),
        .by = id_col)
  }
}



