
# start_time <- as.POSIXct("2023-07-25 00:00:00 UTC")
# end_time <- as.POSIXct("2023-07-31 00:00:00 UTC")

start_time <- as.POSIXct("2023-07-31 00:00:00 UTC")
end_time <- as.POSIXct("2023-07-31 00:00:30 UTC")

tmln <- seq.POSIXt(start_time, end_time, by = "4 secs")
tmpts <- seq.POSIXt(min(tmln), max(tmln), by = "1 secs")


# Effect of NAs ---------------------------------------------------------------
tmpts_NA <- tmpts
tmpts_NA[c(1, 11)] <- NA

test <- snap_times_to_timeline(tmln, tmpts_NA, "nearest"); test
unique(test$tmln_idx)

test_ls <- list(NULL)
test_ls[na.omit(unique(test$tmln_idx))] <- split(test, test$tmln_idx)


tmln_NA <- tmln
tmln_NA[c(2, 5)] <- NA

test <- snap_times_to_timeline(tmln_NA, tmpts, "nearest"); test
unique(test$tmln_idx)

test_ls <- list(NULL)
test_ls[na.omit(unique(test$tmln_idx))] <- split(test, test$tmln_idx)


test <- snap_times_to_timeline(tmln_NA, tmpts_NA, "nearest"); test
test_ls <- list(NULL)
test_ls[na.omit(unique(test$tmln_idx))] <- split(test, test$tmln_idx)
test_ls



test <- snap_times_to_timeline(tmln_NA, tmpts_NA, "latest"); test
test_ls <- list(NULL)
test_ls[na.omit(unique(test$tmln_idx))] <- split(test, test$tmln_idx)
test_ls




# Effect of ordering -----------------------------------------------------------

tmpts_shuff <- sample(tmpts, length(tmpts), replace = FALSE)

test <- snap_times_to_timeline(tmln, tmpts_shuff, "nearest"); test
test_ls <- list(NULL)
test_ls[na.omit(unique(test$tmln_idx))] <- split(test, test$tmln_idx)
test_ls


test <- snap_times_to_timeline(tmln, tmpts_shuff, "latest"); test
test_ls <- list(NULL)
test_ls[na.omit(unique(test$tmln_idx))] <- split(test, test$tmln_idx)
test_ls



# timeline points without timepoints -----------------------------------------------------------

tmln <- seq.POSIXt(start_time, end_time, by = "4 secs")
tmpts <- seq.POSIXt(min(tmln), max(tmln), by = "1 secs")


test <- snap_times_to_timeline(tmln, tmpts[-c(9:14)], "latest"); test
test_ls <- list(NULL)
test_ls[na.omit(unique(test$tmln_idx))] <- split(test, test$tmln_idx)
test_ls




# Proper checking function-----------------------------------------------------------

source("tests/testthat/check_snapped_times.r")


tmln <- seq.POSIXt(start_time, end_time, by = "5 secs")
tmpts <- seq.POSIXt(min(tmln), max(tmln), by = "2 secs")

tmpts_trunc <- tmpts[-c(14:16)]
snapped_idx <- snap_times_to_timeline(tmln,tmpts_trunc , "latest")
check_snapped_times(tmln, tmpts_trunc, snapped_idx, "latest") 




snapped_idx <- snap_times_to_timeline(tmln, tmpts_trunc , "nearest")
check_snapped_times(tmln, tmpts_trunc, snapped_idx, "nearest")

snapped_idx[10, 2] <- 3
check_snapped_times(tmln, tmpts_trunc, snapped_idx, "nearest")




snapped_idx <- snap_times_to_timeline(tmln, tmpts, "nearest")

dt <- tibble(
  tmln = tmln[snapped_idx$tmln_idx],
  tmpts = tmpts[snapped_idx$tmpt_idx]
)

check_snapped_times(dt, tmln_col = tmln, tpts_col = tmpts, "nearest")






tmln <- seq.POSIXt(start_time, end_time, by = "4 secs")
tmpts <- seq.POSIXt(min(tmln), max(tmln), by = "2 secs")

snapped_idx <- snap_times_to_timeline(tmln, tmpts[-c(9:14)], "latest")

dt <- tibble(
  tmln = tmln[snapped_idx$tmln_idx],
  tmpts = tmpts[snapped_idx$tmpt_idx]
)

print(dt, n = 100)

check_snapped_times(dt, tmln_col = tmln, tpts_col = tmpts, "latest")



  


# -------------------------------------------------------------------------------
identical(
  snap_times_to_timeline(tmln, tmpts, "nearest"),
  snap_times_to_timeline_old(tmln, tmpts, "nearest")
)

identical(
  snap_times_to_timeline(tmln, tmpts, "latest"),
  snap_times_to_timeline_old(tmln, tmpts, "latest")
)

bench::mark(
  snap_times_to_timeline(tmln, tmpts, "latest"),
  snap_times_to_timeline_old(tmln, tmpts, "latest")
)



# very large datasets  ----------------------------------------------------------------------------
start_time <- as.POSIXct("2023-06-10 00:00:00 UTC")
end_time <- as.POSIXct("2023-07-31 00:00:30 UTC")

tmln <- seq.POSIXt(start_time, end_time, by = "5 secs")
tmpts <- seq.POSIXt(min(tmln), max(tmln), by = "2 secs")

length(tmln)
length(tmln)

test <- snap_times_to_timeline(tmln, tmpts, "nearest"); test
test <- snap_times_to_timeline(tmln, tmpts, "latest"); test

test_results <- check_snapped_times(tmln, tmpts, snapped_idx = test, "latest")
all(test_results$test)

