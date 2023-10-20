library(move2)
library(purrr)
library(dplyr)
library(sf)
library(tidyr)
library(units)
library(withr)

# read-in test data
input3 <- readRDS(test_path("data", "input3_move2.rds"))
acc_testsets <- readRDS(test_path("data", "acc_testsets.rds"))


# Main `rFunction()`   -----------------------------------------------

# Testing only higher-level functionality, as low-level data processing and
# merging computations are tested below on their specific functions.

test_that("rFunction output is a `move2_loc` object", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")

  input_dt <- readRDS(test_path("data", "move2_loc_1.rds")) |> 
    filter_track_data(individual_local_identifier == "Bateleur_8887") |> 
    slice(1:10)
  
  output <- rFunction(input_dt, usr = usr, pwd = pwd, merging_rule = "latest")
  
  # move2 check
  expect_true(mt_is_move2(output))
  # contains at least one non-empty location point
  expect_true(any(!sf::st_is_empty(output$geometry)))

})


test_that("rFunction doesn't fail if input move2 object is empty", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")

  input_dt <- readRDS(test_path("data", "move2_loc_1.rds")) |>
    filter_track_data(individual_local_identifier == "blah")

  expect_warning(
    rFunction(input_dt, usr = usr, pwd = pwd, merging_rule = "latest"), 
    regexp = "Accelerometer data is not collected for any of the animals"
  )
  
})



test_that("rFunction skips merging if user has no download permissions", {
  
  input_dt <- readRDS(test_path("data", "move2_loc_1.rds"))
  input_trk_dt <- mt_track_data(input_dt)
  input_trk_dt$i_have_download_access <- FALSE
  input_dt <- input_dt |> mt_set_track_data(input_trk_dt)
  
  output <- rFunction(input_dt, usr = usr, pwd = pwd, merging_rule = "latest")
  
  # `acc_dt` list-column should be a list of NULLs
  expect_null(unlist(output$acc_dt))
  # output should be identical to input, bar the attributes (namely the track data)
  expect_equivalent(input_dt, dplyr::select(output, -acc_dt))
  
})



test_that("rFunction skips merging if ACC data is not collected for any of the animals in input", {
  
  input_dt <- readRDS(test_path("data", "move2_loc_2.rds"))
  input_trk_dt <- mt_track_data(input_dt)
  input_trk_dt$sensor.type.ids <- "GPS"
  input_dt <- input_dt |> mt_set_track_data(input_trk_dt)
  
  expect_warning(
    output <- rFunction(input_dt, usr = usr, pwd = pwd, merging_rule = "latest"), 
    regexp = "Accelerometer data is not collected for any of the animals"
  )
  
  suppressWarnings(
    output <- rFunction(input_dt, usr = usr, pwd = pwd, merging_rule = "latest")
  )
  
  # `acc_dt` list-column should be a list of NULLs
  expect_null(unlist(output$acc_dt))
  # output should be identical to input, bar the attributes (namely the track data)
  expect_equivalent(input_dt, dplyr::select(output, -acc_dt))
  
})



test_that("rFunction's option `store_acc_track_info` works", {
  
  withr::local_envvar("APP_ARTIFACTS_DIR"="../../data/output/")
  
  input_dt <- readRDS(test_path("data", "move2_loc_2.rds")) |> 
    filter_track_data(track == "TO_6485") |> 
    slice(1:5)
  
  output <- rFunction(input_dt, usr = usr, pwd = pwd, merging_rule = "latest", 
                      store_acc_track_info = TRUE)
  
  expect_true(!is.null(attr(output, "acc_track_data")))
  
})



# `merge_acc_to_loc`============================================================
test_that("Acc data is merged to location data as expected", {
  
  local_edition(3) # required for `expect_snapshot()`
  withr::local_options(pillar.width = 1000) # needed to keep output consistency for `expect_snapshot()`
  
  set.seed(100)
  acc <- process_acc(acc_testsets$nested_XYZ_raw)
  loc <- mt_filter_per_interval(acc, unit = "60mins") |> dplyr::select(-acc_burst)
  loc_pts <- matrix(c(rnorm(nrow(loc)), rnorm(nrow(loc))), ncol = 2)
  loc$geometry <- sf::st_sfc(apply(loc_pts, 1, sf::st_point, simplify = FALSE))

  # merging rule: "nearest"
  merged_nearest <- merge_acc_to_loc(
    acc, loc, time_col = "timestamp", 
    merging_rule = "nearest") |> 
    dplyr::mutate(
      acc_min_time = purrr::map(acc_dt, ~min(.$timestamp)),
      acc_max_time = purrr::map(acc_dt, ~max(.$timestamp))
    ) |> 
    tidyr::unnest(c(acc_min_time, acc_max_time)) |> 
    dplyr::select(event_id, acc_dt, acc_min_time, acc_max_time)
  
  expect_snapshot(merged_nearest)
  
  
  # merging rule: "latest"
  merged_latest <- merge_acc_to_loc(
    acc, loc, time_col = "timestamp", 
    merging_rule = "latest") |> 
    dplyr::mutate(
      acc_min_time = purrr::map(acc_dt, ~min(.$timestamp)),
      acc_max_time = purrr::map(acc_dt, ~max(.$timestamp))
    ) |> 
    tidyr::unnest(c(acc_min_time, acc_max_time)) |> 
    dplyr::select(event_id, acc_dt, acc_min_time, acc_max_time)
  
  expect_snapshot(merged_latest)

  
  # location events with no ACC data have null in list-column `acc_dt`
  merged <- merge_acc_to_loc(
    acc[c(20:40, 70:90), ], loc, 
    time_col = "timestamp", 
    merging_rule = "latest") |> 
    dplyr::select(event_id, acc_dt) |> 
    dplyr::as_tibble()
  
  expect_snapshot(merged)

})



test_that("No data is lost after acc to location merging", {
  
  acc <- process_acc(acc_testsets$nested_XYZ_raw)
  loc <- mt_filter_per_interval(acc, unit = "60mins") |> dplyr::select(-acc_burst)
  loc_pts <- matrix(c(rnorm(nrow(loc)), rnorm(nrow(loc))), ncol = 2)
  loc$geometry <- sf::st_sfc(apply(loc_pts, 1, sf::st_point, simplify = FALSE))
  
  merged <- merge_acc_to_loc(acc, loc, time_col = "timestamp", merging_rule = "latest")
  
  merged_acc_trk_dt <- mt_track_data(merged$acc_dt[[1]])
  rebuilt_acc <- mt_stack(merged$acc_dt, .track_combine = "merge") |>
    mt_set_track_data(merged_acc_trk_dt)
  
  # no acc location lost after merging
  expect_identical(rebuilt_acc, acc)
  
  # no locations lost after merging
  expect_identical(dplyr::select(merged, -acc_dt), loc)
  
})




# `snap_times_to_timeline()` ===================================================
test_that("time-points are correclty merged to a reference timeline", {
  
  local_edition(3)
  
  tmln <- seq(as.POSIXct("2022-01-01 00:00:00"), as.POSIXct("2022-01-01 00:02:00"), by = "20 secs")
  tmpts <- seq(as.POSIXct("2022-01-01 00:00:01"), as.POSIXct("2022-01-01 00:02:00"), by = "10 secs")
  
  # rule: 'nearest'
  out_nearest <- snap_times_to_timeline(timeline = tmln, timepoints = tmpts, rule = "nearest")
  
  expect_snapshot(
    data.frame(
      timeline = tmln[out_nearest$tmln_idx], 
      tmpoints = tmpts[out_nearest$tmpt_idx])
  )
  
  # rule: 'latest'
  out_latest <- snap_times_to_timeline(timeline = tmln, timepoints = tmpts, rule = "latest")
  
  expect_snapshot(
    data.frame(
      timeline = tmln[out_latest$tmln_idx], 
      tmpoints = tmpts[out_latest$tmpt_idx])
  )
  
  
})



# `process_acc()`  =============================================================
test_that("acc bursts are nested appropriately", {
  
  acc_plain_processed <- process_acc(acc_testsets$plain_XYZ_raw)
  acc_nested_processed <- process_acc(acc_testsets$nested_XY_raw)
  
  # lags are greater that 5 min
  expect_gte(
    min(mt_time_lags(acc_plain_processed), na.rm = TRUE), 
    expected = units::set_units(5, "min")
  )
  # nested dfs with bursts have same nr of cols as nr of acc-axis
  expect_true(all(purrr::map_int(acc_plain_processed$acc_burst, ncol) == 3))
  expect_true(all(purrr::map_int(acc_nested_processed$acc_burst, ncol) == 2))
  
  # number of bursts (i.e. number of rows in output) equals the number of unique
  # `start_timestamp` (if available)
  expect_equal(
    nrow(acc_plain_processed),
    length(unique(acc_testsets$plain_XYZ_raw$start_timestamp))
  )
})


test_that("type of acc data is correctly identified", {
  
  # raw acc
  expect_true(unique(process_acc(acc_testsets$nested_XYZ_raw)$is_acc_raw))
  expect_true(unique(process_acc(acc_testsets$nested_XY_raw)$is_acc_raw))
  expect_true(unique(process_acc(acc_testsets$plain_XYZ_raw)$is_acc_raw))
  expect_false(unique(process_acc(acc_testsets$plain_XYZ_ms2)$is_acc_raw))
  
  # eobs tags
  expect_true(unique(process_acc(acc_testsets$nested_XYZ_raw)$is_acc_eobs))
  expect_true(unique(process_acc(acc_testsets$nested_with_plain_cols)$is_acc_eobs))
  expect_false(unique(process_acc(acc_testsets$plain_XYZ_raw)$is_acc_eobs))
  expect_false(unique(process_acc(acc_testsets$plain_XYZ_ms2)$is_acc_eobs))
  expect_false(unique(process_acc(acc_testsets$plain_with_nested_cols)$is_acc_eobs))
  
})


test_that("acc processing fails when input has no acc info", {
  expect_error(
    process_acc(input3), 
    regexp = "Unable to determine the format of the ACC data"
  )
})


test_that("nrows of acc data originally nested-formated remains unchanged after acc processing", {
  nested_acc <- acc_testsets$nested_X_raw
  expect_equal(nrow(process_acc(nested_acc)), nrow(nested_acc))
})




# `get_acc_format()` ==========================================================
test_that("type of downloaded ACC data format is correctly identified", {
  
  expect_match(get_acc_format(acc_testsets$nested_XYZ_raw), "nested")
  expect_match(get_acc_format(acc_testsets$nested_XY_raw), "nested")
  expect_match(get_acc_format(acc_testsets$nested_with_plain_cols), "nested")
  expect_match(get_acc_format(acc_testsets$plain_XYZ_raw), "plain")
  expect_match(get_acc_format(acc_testsets$plain_XYZ_ms2), "plain")
  expect_match(get_acc_format(acc_testsets$plain_with_nested_cols), "plain")
  expect_match(get_acc_format(input3), "undetermined")
   
})



# `mark_time_bursts()`  ========================================================
test_that("acc burst identification output has same length as input", {
  
  actual <- mark_time_bursts(acc_testsets$plain_XYZ_raw$timestamp)
  expect_length(actual, length(acc_testsets$plain_XYZ_raw$timestamp))
  
})

test_that("burst identification works", {
  
  burst1 <- seq.POSIXt(as.POSIXct("2023-09-01 16:00:00"), as.POSIXct("2023-09-01 16:01:00"), units = "sec", by = 0.6)
  burst2 <- seq.POSIXt(as.POSIXct("2023-09-01 17:00:00"), as.POSIXct("2023-09-01 17:01:00"), units = "sec", by = 0.2)
  burst3 <- seq.POSIXt(as.POSIXct("2023-09-01 17:02:00"), as.POSIXct("2023-09-01 17:03:00"), units = "sec", by = 0.8)
  bursts <- c(burst1, burst2, burst3)
   
  expect_identical(
    mark_time_bursts(bursts), 
    c(rep(1, length(burst1)), rep(2, length(burst2)), rep(3, length(burst3)))
  )
})



# `acc_string_to_matrix()`  ===================================================
test_that("`acc_string_to_matrix()` works correctly for different number of axis", {

  sapply(c(1, 2, 3), \(naxis){
    acc_str_dt <- generate_acc_string(naxis)
    acc_str <- acc_str_dt$str
    axis_id <- acc_str_dt$axis_id
    expected <- acc_str_dt$expected
    expect_equal(acc_string_to_matrix(acc_str, axis_id), expected)  
  })
  
})


test_that("`acc_string_to_matrix()` works for different whitespaces between numbers", {
  
  sapply(c(" ", "  ", "   "), \(sep){
    acc_str_dt <- generate_acc_string(naxis = 2, sep)
    acc_str <- acc_str_dt$str
    axis_id <- acc_str_dt$axis_id
    expected <- acc_str_dt$expected
    expect_equal(acc_string_to_matrix(acc_str, axis_id), expected)  
  })
  
})



# mt_as_move2_tibble() =========================================================
test_that("mt_as_move2_tibble() works as expected", {
  
  n <- 5
  move2_df <- mt_as_move2(
    data.frame(x = cumsum(rnorm(n)), y = cumsum(rnorm(n)), time = seq(n), track = "a"),
    coords = c("x", "y"), time_column = "time",
    track_id_column = "track"
  )
  
  move2_tbldf <- mt_as_move2_tibble(move2_df)
  expect_true(inherits(move2_tbldf, "tbl_df"))
  expect_true(inherits(mt_track_data(move2_tbldf), "tbl_df"))
  
})



# `time_to_decimal_hours()` ====================================================
test_that("conversion from data-time to decimal hours works", {
  expect_equal(time_to_decimal_hours(as.POSIXct("2023-09-01 17:03:00")), 17 + 3/60)
  expect_equal(time_to_decimal_hours(as.POSIXct("2023-09-01 12:12:12")), 12 + 12/60 + 12/3600)
  expect_equal(time_to_decimal_hours(as.POSIXct("2023-09-01 00:00:55")), 55/3600)
})


test_that("time_to_decimal_hours() throws expected error", {
  expect_error(time_to_decimal_hours(234), "not of type POSIXct")
  expect_error(time_to_decimal_hours("bla"), "not of type POSIXct")
})



 
