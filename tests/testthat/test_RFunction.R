library('move2')

test_data <- test_data("input3_move2.rds") #file must be move2!
acc_testsets <- readRDS(test_path("data", "acc_testsets.rds"))

test_that("happy path", {
  actual <- rFunction(data = test_data, usr = usr, pwd = pwd, sdk = "unit test", year = 2005)
  expect_equal(unique(lubridate::year(mt_time(actual))), 2005)
})

test_that("year not included", {
  actual <- rFunction(data = test_data, usr = usr, pwd = pwd, sdk = "unit test", year = 2023)
  expect_null(actual)
})




# ACC data format identification -----------------------------------------------
test_that("type of downloaded ACC data format is correctly identified", {
  
  expect_match(get_acc_format(acc_testsets$nested_XYZ_raw), "nested")
  expect_match(get_acc_format(acc_testsets$nested_XY_raw), "nested")
  expect_match(get_acc_format(acc_testsets$nested_with_plain_cols), "nested")
  expect_match(get_acc_format(acc_testsets$plain_XYZ_raw), "plain")
  expect_match(get_acc_format(acc_testsets$plain_XYZ_ms2), "plain")
  expect_match(get_acc_format(acc_testsets$plain_with_nested_cols), "plain")
   
})


# burst identification ---------------------------------------------------------
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






 
