library('move2')
library("units")

test_data <- test_data("input3_move2.rds") #file must be move2!
acc_testsets <- readRDS(test_path("data", "acc_testsets.rds"))


# Main `rFunction()`   -----------------------------------------------

# test_that("happy path", {
#   actual <- rFunction(data = test_data, usr = usr, pwd = pwd)
#   expect_equal(unique(lubridate::year(mt_time(actual))), 2005)
# })
# 
# test_that("year not included", {
#   actual <- rFunction(data = test_data, usr = usr, pwd = pwd)
#   expect_null(actual)
# })



# `preprocess_acc()`  -----------------------------------------------
test_that("acc bursts are nested appropriately", {
  
  acc_processed <- preprocess_acc(acc_testsets$plain_XYZ_raw)
  
  # lags are greater that 5 min
  expect_gte(
    min(mt_time_lags(acc_processed), na.rm = TRUE), 
    expected = set_units(5, "min")
  )
  # nested dfs with bursts have same # of cols as # of acc-axis
  expect_true(all(map_int(acc_processed$acc_bursts, ncol) == 3))
  
})


test_that("there are acc summaries for each active acc-axis", {
  
  expected_names <- names(preprocess_acc(acc_testsets$nested_XYZ_raw))
  expect_identical(
    grep("(mean)|(var)_acc", expected_names, value = TRUE),
    c("mean_acc_x", "mean_acc_y", "mean_acc_z", "var_acc_x", "var_acc_y", "var_acc_z")
  )
    
  expected_names <- names(preprocess_acc(acc_testsets$nested_XY_raw))
  expect_identical(
    grep("(mean)|(var)_acc", expected_names, value = TRUE),
    c("mean_acc_x", "mean_acc_y", "var_acc_x", "var_acc_y")
  )
  
  expected_names <- names(preprocess_acc(acc_testsets$nested_X_raw))
  expect_identical(
    grep("(mean)|(var)_acc", expected_names, value = TRUE),
    c("mean_acc_x", "var_acc_x")
  )
  
})


test_that("type of acc data is correctly identified", {
  
  # raw acc
  expect_true(unique(preprocess_acc(acc_testsets$nested_XYZ_raw)$is_acc_raw))
  expect_true(unique(preprocess_acc(acc_testsets$nested_XY_raw)$is_acc_raw))
  expect_true(unique(preprocess_acc(acc_testsets$plain_XYZ_raw)$is_acc_raw))
  expect_false(unique(preprocess_acc(acc_testsets$plain_XYZ_ms2)$is_acc_raw))
  
  # eobs tags
  expect_true(unique(preprocess_acc(acc_testsets$nested_XYZ_raw)$is_acc_eobs))
  expect_true(unique(preprocess_acc(acc_testsets$nested_with_plain_cols)$is_acc_eobs))
  expect_false(unique(preprocess_acc(acc_testsets$plain_XYZ_raw)$is_acc_eobs))
  expect_false(unique(preprocess_acc(acc_testsets$plain_XYZ_ms2)$is_acc_eobs))
  expect_false(unique(preprocess_acc(acc_testsets$plain_with_nested_cols)$is_acc_eobs))
  
})



# `get_acc_format()`: ACC data format identification -----------------------------------------------
test_that("type of downloaded ACC data format is correctly identified", {
  
  expect_match(get_acc_format(acc_testsets$nested_XYZ_raw), "nested")
  expect_match(get_acc_format(acc_testsets$nested_XY_raw), "nested")
  expect_match(get_acc_format(acc_testsets$nested_with_plain_cols), "nested")
  expect_match(get_acc_format(acc_testsets$plain_XYZ_raw), "plain")
  expect_match(get_acc_format(acc_testsets$plain_XYZ_ms2), "plain")
  expect_match(get_acc_format(acc_testsets$plain_with_nested_cols), "plain")
   
})


# `mark_time_bursts()`: burst identification ---------------------------------------------------------
test_that("burst identification output has same length as input", {
  
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






 
