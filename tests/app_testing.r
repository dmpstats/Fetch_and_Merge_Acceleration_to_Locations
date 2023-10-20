# ------------------- #
#       Preamble 
# ------------------- #
library(fs)
library(jsonlite)


# read input datasets for testing
data_path <- "data/raw/"
test_inputs <- fs::dir_map(data_path, readRDS)
names(test_inputs) <- basename(path_ext_remove(dir_ls(data_path)))

large_gps_nam <- readRDS("dev/vult_nam_large.rds")

# movebank credentials stored in HOME .Renviron file, set up via `usethis::edit_r_environ()`
usr <- Sys.getenv("vult_usr")
pwd <- Sys.getenv("vult_pwd")

#options(pillar.width = Inf)


# ------------------------- #
#  Automated Unit testing  
# ------------------------- #

testthat::test_file("tests/testthat/test_RFunction.R")




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
    jsonlite::toJSON(new_app_config,  pretty = TRUE, auto_unbox = TRUE), 
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


# ----- AVulture SOP Namibia vultures  ----------
run_sdk(test_inputs$gps_vult_nam, usr, pwd)
output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.rds"); artifact_acc_dwnld


# ----- GAIA vultures from africa  ----------------
run_sdk(test_inputs$gps_vult_gaia, usr, pwd)
output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.rds"); artifact_acc_dwnld


# ----- high frequency ACC  ----------
run_sdk(test_inputs$gps_high_freq, usr, pwd)
output <- readRDS("data/output/output.rds"); output
output$acc_dt[[1]]


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


# store acc track info
run_sdk(test_inputs$gps_vult_nam, usr, pwd, store_acc_track_info = TRUE)
output <- readRDS("data/output/output.rds"); output
attributes(output)


# thin acc data using the time filter 
run_sdk(test_inputs$gps_vult_nam, usr, pwd, acc_timefilter = 15)
output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.rds"); artifact_acc_dwnld




