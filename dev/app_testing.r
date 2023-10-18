# ------------------- #
#       Preamble 
# ------------------- #
library(fs)
library(jsonlite)

source("RFunction.R")

# read input datasets for testing
data_path <- "data/raw/"
test_inputs <- fs::dir_map(data_path, readRDS)
names(test_inputs) <- basename(path_ext_remove(dir_ls(data_path)))

large_gps_nam <- readRDS("dev/vult_nam_large.rds")

# movebank credentials stored in HOME .Renviron file
usr <- Sys.getenv("vult_usr")
pwd <- Sys.getenv("vult_pwd")

options(pillar.width = Inf)

default_app_config <- jsonlite::fromJSON("app-configuration-secrets.json"); default_app_config



# ------------------------- #
#  Automated Unit testing  
# ------------------------- #

testthat::test_file("tests/testthat/test_RFunction.R")




# ----------------------- #
#   MoveApps SDK testing  
# ----------------------- #

# Different datasets ----------------------------------------------------------

# ----- Vultures South Africa VfA MPIAB  ----------
saveRDS(test_inputs$gps_vult_sa, "data/raw/input_sdk_testing.rds")
source("sdk.R")

output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.csv"); artifact_acc_dwnld


# ----- AVulture SOP Namibia vultures  ----------
saveRDS(test_inputs$gps_vult_nam, "data/raw/input_sdk_testing.rds")
source("sdk.R")

output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.csv"); artifact_acc_dwnld

# ----- GAIA vultures from africa  ----------------
saveRDS(test_inputs$gps_vult_gaia, "data/raw/input_sdk_testing.rds")
source("sdk.R")

output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.csv"); artifact_acc_dwnld


# ----- high frequency ACC  ----------
saveRDS(test_inputs$gps_high_freq, "data/raw/input_sdk_testing.rds")
source("sdk.R")

output <- readRDS("data/output/output.rds"); output
output$acc_dt[[1]]
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.csv"); artifact_acc_dwnld


# ----- low frequency ACC  ----------
saveRDS(test_inputs$gps_low_freq, "data/raw/input_sdk_testing.rds")
source("sdk.R")

output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.csv"); artifact_acc_dwnld


# ----- mixed acc availability  ----------
saveRDS(test_inputs$gps_mixed_acc, "data/raw/input_sdk_testing.rds")
source("sdk.R")

output <- readRDS("data/output/output.rds"); output

# ----- ACC not collected  ----------
saveRDS(test_inputs$gps_without_acc, "data/raw/input_sdk_testing.rds")
source("sdk.R")

output <- readRDS("data/output/output.rds"); output


# ----- App output from test workflow   ----------
saveRDS(test_inputs$gps_test_workflow_app_output, "data/raw/input_sdk_testing.rds")
source("sdk.R")

output <- readRDS("data/output/output.rds"); output


# ----- Large dataset  ----------
saveRDS(large_gps_nam, "data/raw/input_sdk_testing.rds")
source("sdk.R")

output <- readRDS("data/output/output.rds"); output


# ----- MoveApps default inputs  ----------

# Throws **error** due to non-accepted study license terms
saveRDS(test_inputs$input1, "data/raw/input_sdk_testing.rds")
source("sdk.R")

# rFunction(data = test_inputs$input1, usr = usr, pwd = pwd, merging_rule = "latest")


# acc not collected
saveRDS(test_inputs$input2, "data/raw/input_sdk_testing.rds")
source("sdk.R")
output <- readRDS("data/output/output.rds"); output

# acc not collected
saveRDS(test_inputs$input3, "data/raw/input_sdk_testing.rds")
source("sdk.R")
output <- readRDS("data/output/output.rds"); output

# acc not collected
saveRDS(test_inputs$input4, "data/raw/input_sdk_testing.rds")
source("sdk.R")
output <- readRDS("data/output/output.rds"); output




# Different App settings (inputs) --------------------------------------------------

saveRDS(test_inputs$gps_vult_nam, "data/raw/input_sdk_testing.rds")

# Merging rule
new_app_config <- default_app_config
new_app_config$merging_rule <- "nearest"
write(
  jsonlite::toJSON(new_app_config,  pretty = TRUE, auto_unbox = TRUE), 
  file = "app-configuration-secrets.json"
)
source("sdk.R")

output <- readRDS("data/output/output.rds"); output


# store acc track info
new_app_config <- default_app_config
new_app_config$store_acc_track_info <- TRUE
write(
  jsonlite::toJSON(new_app_config,  pretty = TRUE, auto_unbox = TRUE), 
  file = "app-configuration-secrets.json"
)
source("sdk.R")

output <- readRDS("data/output/output.rds"); output

attributes(output)


# thin acc data using the time filter 
new_app_config <- default_app_config
new_app_config$acc_timefilter <- 15
write(
  jsonlite::toJSON(new_app_config,  pretty = TRUE, auto_unbox = TRUE), 
  file = "app-configuration-secrets.json"
)
source("sdk.R")

output <- readRDS("data/output/output.rds"); output
artifact_acc_dwnld <- readRDS("data/output/downloaded_acc_data.csv"); artifact_acc_dwnld



# reset to default config ------------------------------------------------------
write(
  jsonlite::toJSON(default_app_config,  pretty = TRUE, auto_unbox = TRUE), 
  file = "app-configuration-secrets.json"
)
