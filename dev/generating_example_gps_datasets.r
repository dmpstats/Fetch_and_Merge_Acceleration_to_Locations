# ------------------------------------------------- #
#    Get and store example accelerometer datasets   #
# ------------------------------------------------- #1

# Preamble --------------------------------------------------------------------

library(keyring)
library(move2)
library(dplyr)
library(purrr)
library(httr2)
library(stringr)
library(bench)

# movebank credentials stored in HOME .Renviron file
usr <- Sys.getenv("vult_usr")
pwd <- Sys.getenv("vult_pwd")

options(pillar.width = Inf)

# set up Movebank credentials to local environment
move2::movebank_store_credentials(
  username = usr, 
  password = psw
)

keyring::key_list()

# Get all studies available with acceleration data -----------------------------

## download study info for all studies with access to download the data
studies_list <- movebank_download_study_info(i_have_download_access = TRUE)


## filter studis with gps and with full access
studies_list <- studies_list |> 
  filter(
    there_are_data_which_i_cannot_see  == FALSE,
    str_detect(sensor_type_ids, "(gps)|(GPS)")
  )

studies_list |> dplyr::select(name, id)

# Get info on animals in each study --------------------------------------------
animal_info <- studies_list |> 
  pmap(\(id, ...){
    tryCatch(
      movebank_retrieve(
        entity_type = "individual",
        study_id = id, 
        omit_derived_data = FALSE
      ),
      error = function(e){ e$message}
    )
  }, .progress = TRUE)

names(animal_info) <- studies_list$id

animal_info

## subset to only animals
animal_info_df <- animal_info |> 
  keep(is.data.frame) |> 
  list_rbind(names_to = "study_id")



# Example GPS data sets for high and low frequency GPS ------------------

# calculate event frequency per animals
event_freq_animals <- animal_info_df |> 
  filter(str_detect(sensor_type_ids, "(a|A)cceleration")) |> 
  mutate(time_span = difftime(timestamp_end, timestamp_start, units = "weeks")) |> 
  filter(time_span > 50) |> 
  mutate(event_freq = as.numeric(time_span)/as.numeric(number_of_events)) |> 
  arrange(event_freq)

  
# pick high frequency study and select 2 animals
high_freq_animals <- event_freq_animals |> 
  filter(study_id == 2065208399) |>  #893458555) |> 
  arrange(desc(timestamp_end)) |> 
  slice(1:2)


high_freq <- move2::movebank_download_study(
  study_id = as.numeric(unique(high_freq_animals$study_id)), 
  sensor_type_id = "gps",
  individual_id = high_freq_animals$id,
  timestamp_start = max(high_freq_animals$timestamp_end) - lubridate::days(3),
  timestamp_end = max(high_freq_animals$timestamp_end),
)



# pick low frequency study and select 2 animals
event_freq_animals |> 
  arrange(desc(event_freq)) |> 
  slice(11:20)


low_freq_animals <- event_freq_animals |> 
  filter(study_id == 10449318) |> 
  arrange(desc(timestamp_end)) |> 
  slice(9:10)


low_freq <- move2::movebank_download_study(
  study_id = as.numeric(unique(low_freq_animals$study_id)), 
  sensor_type_id = "gps",
  individual_id = low_freq_animals$id,
  timestamp_start = max(low_freq_animals$timestamp_end) - lubridate::days(3),
  timestamp_end = max(low_freq_animals$timestamp_end),
)




# Example GPS data set with mix of individuals with and without acc ------------

animal_info_df |> 
  group_by(study_id) |> 
  reframe(unique(sensor_type_ids)) |> 
  print(n = 150)


mixed_animals <- animal_info_df |> 
  filter(study_id == 985143423 ) |> # 1562253659 ) #|>
  arrange(timestamp_end) |> 
  slice(123:126)
  #slice(4:6)


mixed_acc <- move2::movebank_download_study(
  study_id = as.numeric(unique(mixed_animals$study_id)), 
  sensor_type_id = "gps",
  individual_id = mixed_animals$id,
  timestamp_start = max(mixed_animals$timestamp_end) - lubridate::days(15),
  timestamp_end = max(mixed_animals$timestamp_end),
)

mixed_acc


# Example GPS data set without any acc -----------------------------------------

animal_info_df |> 
  filter(str_detect(sensor_type_ids, "(a|A)cceleration", negate = TRUE)) |> 
  arrange(desc(number_of_events)) |> 
  print(n = 100)


without_acc_animals <- animal_info_df |> 
  filter(
    study_id == 2217728245, 
    str_detect(sensor_type_ids, "(a|A)cceleration", negate = TRUE)
    ) |>
  arrange(timestamp_end) |> 
  print(n = 100) |> 
  slice(15:17)
  

without_acc <- move2::movebank_download_study(
  study_id = as.numeric(unique(without_acc_animals$study_id)), 
  sensor_type_id = "gps",
  individual_id = without_acc_animals$id,
  timestamp_start = max(without_acc_animals$timestamp_end) - lubridate::days(3),
  timestamp_end = max(without_acc_animals$timestamp_end),
)

without_acc



# Example GPS data sets for a vulture studies (GPS *and* ACC )------------------

# study info on vulture studies
vult_studies <- studies_list |>
  filter(str_detect(name, "(v|V)ult"))

# --- South Africa vultures VfA MPIAB
vult_sa_id <- vult_studies |> 
  filter(str_detect(name, "VfA MPIAB")) |> 
  pull(id)

set.seed(112)
vult_sa_animals <- animal_info_df |> 
  filter(
    study_id == vult_sa_id
  ) |> 
  slice_sample(n=3)

# dowload last 2 days of data
vult_sa <- move2::movebank_download_study(
  study_id = as.numeric(unique(vult_sa_animals$study_id)), 
  sensor_type_id = "gps",
  individual_id = vult_sa_animals$id,
  timestamp_start = max(vult_sa_animals$timestamp_end) - lubridate::days(10),
  timestamp_end = max(vult_sa_animals$timestamp_end)
)

vult_sa
summary(mt_time_lags(vult_sa))


# --- GAIA vultures from africa
vult_gaia_id <- vult_studies |> 
  filter(str_detect(name, "GAIA")) |> 
  pull(id)

# randomly choose 3 animals
set.seed(112)
gaia_animals <- animal_info_df |> 
  filter(
    study_id == vult_gaia_id,
    !is.na(sensor_type_ids),
    timestamp_end > as.POSIXct("2023-01-01 00:00:01 UTC")
  ) |> 
  slice_sample(n=4)

vult_gaia <- move2::movebank_download_study(
  study_id = as.numeric(unique(gaia_animals$study_id)), 
  sensor_type_id = "gps",
  individual_id = gaia_animals$id,
  timestamp_start = max(gaia_animals$timestamp_end) - lubridate::days(3),
  timestamp_end = max(gaia_animals$timestamp_end)
)

vult_gaia
summary(mt_time_lags(vult_gaia))



# --- AVulture SOP Namibia vultures
namsop_id <- vult_studies |> 
  filter(str_detect(name, "AVulture Namibia")) |> 
  pull(id) |> 
  as.character()

# randomly choose 3 animals
set.seed(112)
namsop_animals <- animal_info_df |> 
  filter(
    study_id == namsop_id,
    !is.na(sensor_type_ids),
    str_detect(sensor_type_ids, "acceleration"),
    timestamp_end > as.POSIXct("2023-01-01 00:00:01 UTC")
  ) |> 
  slice_sample(n=2)

vult_nam <- move2::movebank_download_study(
  study_id = as.numeric(unique(namsop_animals$study_id)), 
  sensor_type_id = "gps",
  individual_id = namsop_animals$id,
  timestamp_start = max(namsop_animals$timestamp_end) - lubridate::days(3),
  timestamp_end = max(namsop_animals$timestamp_end)
)

vult_nam
summary(mt_time_lags(vult_nam))







# Write-out datasets ---------------------------------------------------------

readr::write_rds(vult_sa, file = "data/raw/gps_vult_sa.rds")
readr::write_rds(vult_gaia, file = "data/raw/gps_vult_gaia.rds")
readr::write_rds(vult_nam, file = "data/raw/gps_vult_nam.rds")
readr::write_rds(high_freq, file = "data/raw/gps_high_freq.rds")
readr::write_rds(low_freq, file = "data/raw/gps_low_freq.rds")
readr::write_rds(mixed_acc, file = "data/raw/gps_mixed_acc.rds")
readr::write_rds(without_acc, file = "data/raw/gps_without_acc.rds")





