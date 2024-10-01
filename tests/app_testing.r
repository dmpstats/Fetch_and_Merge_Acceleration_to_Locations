# NOTE: Some of the datasets used here were created earlier via
# "dev/generating_example_gps_datasets.r". If script is being used for the first
# time since cloning the code repository from GitHub, these datasets need to be
# re-generated once.

# ------------------- #
#       Preamble 
# ------------------- #
library(fs)
library(jsonlite)

# Read input datasets for testing
data_path <- "data/raw/"
test_inputs <- fs::dir_map(data_path, readRDS)
names(test_inputs) <- basename(path_ext_remove(dir_ls(data_path)))

large_gps_nam <- readRDS("dev/vult_nam_large.rds")

# movebank credentials stored in HOME .Renviron file, set up via `usethis::edit_r_environ()`
# CAUTION: DO NOT USE YOUR ACTUAL LOGIN DETAILS HERE, OTHERWISE YOU MAY EXPOSE YOUR CREDENTIALS INADVERTENTLY
usr <- Sys.getenv("vult_usr")
pwd <- Sys.getenv("vult_pwd")

#options(pillar.width = Inf)

# helper function to check if acc was merged to locations correctly (based on) timestamps)
source("tests/testthat/check_snapped_times.r")



# ------------------------- #
#  Automated Unit testing  
# ------------------------- #

testthat::test_file("tests/testthat/test_RFunction.R")



# ------------------------- #
#  Interactive testing 
# ------------------------- #

# testing handling of missing sensor data
out <- rFunction(
  data = test_inputs$gps_vult_nam |> mt_as_event_attribute(sensor_type_ids), 
  usr = usr, 
  pwd = pwd,
  acc_timefilter = 3
)



out <- rFunction(
  data = test_inputs$gps_without_acc, 
  usr = usr, 
  pwd = pwd,
  acc_timefilter = 3
)


# ----------------------- #
#   MoveApps SDK testing  
# ----------------------- #

# helper to run SDK testing with different datasets and settings
run_sdk <- function(data, usr, pwd,  
                    merging_rule = "latest", 
                    store_acc_track_info = FALSE, 
                    acc_timefilter = 0){
  
  # get environmental variables specified in .env
  dotenv::load_dot_env(".env")
  app_config_file <- Sys.getenv("CONFIGURATION_FILE")
  source_file <- Sys.getenv("SOURCE_FILE")
  
  # store default app configuration
  dflt_app_config <- jsonlite::fromJSON(app_config_file)
  # get default input data
  dflt_dt <- readRDS(source_file)
  
  # set configuration to specified inputs
  new_app_config <- dflt_app_config
  new_app_config$usr <- usr
  new_app_config$pwd <- pwd
  new_app_config$merging_rule <- merging_rule
  new_app_config$store_acc_track_info <- store_acc_track_info
  new_app_config$acc_timefilter <- acc_timefilter
  
  # overwrite config file with current inputs
  write(
    jsonlite::toJSON(new_app_config, pretty = TRUE, auto_unbox = TRUE), 
    file = app_config_file
  )
  
  # overwrite app's source file with current input data
  saveRDS(data, source_file)
  
  # run SDK for the current settings
  try(source("sdk.R"))
  
  # reset to default config and data
  write(
    jsonlite::toJSON(dflt_app_config,  pretty = TRUE, auto_unbox = TRUE), 
    file = app_config_file
  )
  saveRDS(dflt_dt, source_file)
  
  invisible()
}



# Different datasets ----------------------------------------------------------

# ----- Vultures South Africa VfA MPIAB  ----------
run_sdk(test_inputs$gps_vult_sa, usr, pwd)
output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.rds"); artifact_acc_dwnld

check_merged_times(output, rule = "latest") |> 
  pull(test) |> 
  all(na.rm = TRUE)


# ----- AVulture SOP Namibia vultures  ----------
run_sdk(test_inputs$gps_vult_nam, usr, pwd)
output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.rds"); artifact_acc_dwnld

check_merged_times(output, rule = "latest") |> 
  pull(test) |> 
  all(na.rm = TRUE)



# ----- GAIA vultures from africa  ----------------
run_sdk(test_inputs$gps_vult_gaia, usr, pwd)
output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.rds"); artifact_acc_dwnld

check_merged_times(output, rule = "latest") |> 
  pull(test) |> 
  all(na.rm = TRUE)


# ----- high frequency ACC  ----------
run_sdk(test_inputs$gps_high_freq, usr, pwd)
output <- readRDS("data/output/output.rds"); output
output$acc_dt[[1]]

check_merged_times(output, rule = "latest") |> 
  pull(test) |> 
  all(na.rm = TRUE)


# ----- low frequency ACC  ----------
run_sdk(test_inputs$gps_low_freq, usr, pwd)
output <- readRDS("data/output/output.rds"); output


# ----- mixed acc availability  ----------
run_sdk(test_inputs$gps_mixed_acc, usr, pwd)
output <- readRDS("data/output/output.rds"); output


# ----- ACC not collected  ----------
run_sdk(test_inputs$gps_without_acc, usr, pwd)
output <- readRDS("data/output/output.rds"); output


# ----- App output from test workflow   ----------
run_sdk(test_inputs$gps_test_workflow_app_output, usr, pwd)
output <- readRDS("data/output/output.rds"); output


# ----- Large dataset  ----------
run_sdk(test_inputs$large_gps_nam, usr, pwd)
output <- readRDS("data/output/output.rds"); output


# ----- MoveApps default inputs  ----------

# Throws **error** due to non-accepted study license terms
run_sdk(test_inputs$input1, usr, pwd)
# rFunction(data = test_inputs$input1, usr = usr, pwd = pwd, merging_rule = "latest")


# acc not collected
run_sdk(test_inputs$input2, usr, pwd)
output <- readRDS("data/output/output.rds"); output

# acc not collected
run_sdk(test_inputs$input3, usr, pwd)
output <- readRDS("data/output/output.rds"); output

# acc not collected
run_sdk(test_inputs$input4, usr, pwd)
output <- readRDS("data/output/output.rds"); output




# Different App settings (inputs) --------------------------------------------------

# Merging rule
run_sdk(test_inputs$gps_vult_nam, usr, pwd, merging_rule = "nearest")
output <- readRDS("data/output/output.rds"); output

check_merged_times(output, rule = "nearest") |> 
  pull(test) |> 
  all(na.rm = TRUE)



# store acc track info
run_sdk(test_inputs$gps_vult_nam, usr, pwd, store_acc_track_info = TRUE)
output <- readRDS("data/output/output.rds"); output
attributes(output)


# thin acc data using the time filter 
run_sdk(test_inputs$gps_vult_nam, usr, pwd, acc_timefilter = 15)
output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.rds"); artifact_acc_dwnld




