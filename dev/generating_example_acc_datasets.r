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

# movebank credentials stored in HOME .Renviron file, set up via `usethis::edit_r_environ()`
# CAUTION: DO NOT USE YOUR ACTUAL LOGIN DETAILS HERE, OTHERWISE YOU MAY EXPOSE YOUR CREDENTIALS INADVERTENTLY
usr <- Sys.getenv("vult_usr")
pwd <- Sys.getenv("vult_pwd")

options(pillar.width = Inf)

# set up Movebank credentials to local environment
move2::movebank_store_credentials(
  username = usr, 
  password = pwd
)

keyring::key_list()

# Get all studies available with acceleration data -----------------------------

## download study info for all studies with access to download the data
studies_list <- movebank_download_study_info(i_have_download_access = TRUE)

## filter acceleration on vulture studies only
studies_acc_list <- studies_list |> 
  dplyr::filter(
    str_detect(sensor_type_ids, "Acceleration")
    #str_detect(name, "(V|v)ul")
    )


studies_acc_list |> dplyr::select(name, id)

# Get info on animals in each study --------------------------------------------
animal_info <- studies_acc_list |> 
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

names(animal_info) <- studies_acc_list$id

## subset to only animals with acceleration data
animal_acc_info <- animal_info |> 
  keep(is.data.frame) |> 
  list_rbind(names_to = "study_id") |> 
  dplyr::filter(str_detect(sensor_type_ids, "(a|A)cceleration"))
  #map(~dplyr::filter(., str_detect(.$sensor_type_ids, "(a|A)cceleration")))
  


# Download last 3 days of acc data for each individual -------------------------
i <- 0
acc_dt_dwnl <- animal_acc_info |> 
  rowwise() |> 
  mutate(
    acc_dwnl = pmap(list(study_id, id, timestamp_end), \(st_id, in_id, time_end, ...){
      i <<- i + 1
      print(i)
      
      try(
        move2::movebank_download_study(
          study_id = as.numeric(st_id), 
          sensor_type_id = "acceleration",
          individual_id = in_id,
          timestamp_start = time_end - lubridate::days(2),
          timestamp_end = time_end
        ), silent = TRUE
      )
    })
  ) |> 
  ungroup()



# Drop individuals with unavailable data (due to license)  ---------------------
acc_dt <- acc_dt_dwnl |> 
  mutate(acc_dwnl_error = map_lgl(acc_dwnl, \(x){any(class(x) == "try-error")})) |> 
  filter(acc_dwnl_error != TRUE) 



# Classify type of ACC data format ---------------------------------------------

source("dev/get_acc_format.r")

acc_dt <- acc_dt |> 
  mutate(acc_output_fmt = map_chr(acc_dwnl, get_acc_format))

acc_dt |> 
  filter(acc_output_fmt == "undetermined")



# Look for different sub-formats  ---------------------------------------------

# Nested format
acc_nested <- acc_dt |> 
  filter(acc_output_fmt == "nested")

## different number of active axes
nested_axes <- map_dfr(acc_nested$acc_dwnl, \(x){
  as.data.frame(x) |> 
    select(ends_with("acceleration_axes")) |> 
    distinct() 
  }) |> 
  pull()

acc_nested$acc_dwnl[which(nested_axes == "X")[5]]
acc_nested$acc_dwnl[which(nested_axes == "XY")[10]]

## presence of plain-format columns despite being in nested format
map(acc_nested$acc_dwnl, \(x){
  as.data.frame(x) |> 
    select(matches("acceleration_(raw_)?[xyz]")) |> 
    tidyr::drop_na()
})

## check for differently named "nested" ACC columns. According to Movebank
## Attribute Dictionary, nested acc values are stored in columns "*accelerations*
## (plural)
map(acc_nested$acc_dwnl, \(x){
  as.data.frame(x) |> 
    select(matches("accelerations")) |> 
    names()
})



# Plain format
acc_plain <- acc_dt |> 
  filter(acc_output_fmt == "plain")

## check for differently named plain ACC columns. According to Movebank
## Attribute Dictionary, axis-specific values are stored in columns "*acceleration*
## (singular)
map(acc_plain$acc_dwnl, \(x){
  as.data.frame(x) |> 
    select(matches("acceleration_(raw_)?[xyz]")) |>
    names()
}) 

# most cases provide raw values, but there are a few exceptions with ACC in m/s^2
acc_plain$acc_dwnl[140:152]



# Create large datasets (3-axes)  ---------------------------------------------

animal_acc_info |> 
  arrange(desc(number_of_events)) |> 
  print(n = 50)

nested_XYZ_raw_large <- move2::movebank_download_study(
  study_id = 1393954358, 
  sensor_type_id = "acceleration",
  individual_id = 1393966589,
  timestamp_start = as.POSIXct("2023-09-13 05:00:41") - lubridate::days(15),
  timestamp_end = as.POSIXct("2023-09-13 05:00:41")
)

plain_XYZ_raw_large <- move2::movebank_download_study(
  study_id = 1605797471, 
  sensor_type_id = "acceleration",
  individual_id = 1792019253,
  timestamp_start = as.POSIXct("2019-05-25 13:51:26") - lubridate::days(15),
  timestamp_end = as.POSIXct("2019-05-25 13:51:26")
)



# Write-out datasets ---------------------------------------------------------

readr::write_rds(
  list(
    nested_XYZ_raw = acc_nested$acc_dwnl[[10]], 
    nested_XY_raw = acc_nested$acc_dwnl[[which(nested_axes == "XY")[2]]],
    nested_X_raw = acc_nested$acc_dwnl[[which(nested_axes == "X")[5]]],
    nested_with_plain_cols = acc_nested$acc_dwnl[[105]],
    plain_XYZ_raw = acc_plain$acc_dwnl[[2]],
    plain_XYZ_ms2 = acc_plain$acc_dwnl[[143]], 
    plain_with_nested_cols = acc_dt$acc_dwnl[[243]]
  ), file = "dev/acc_testsets.rds"
)



readr::write_rds(
  list(
    nested_XYZ_raw_large = nested_XYZ_raw_large,
    plain_XYZ_raw_large = plain_XYZ_raw_large
  ), file = "dev/acc_testsets_large.rds"
)












movebank_retrieve(entity_type = "sensor", tag_study_id = 352166254)
movebank_retrieve(entity_type = "study_attribute", study_id = 352166254, sensor_type_id= c(653, 2365683 ))


animal_acc_info[str_detect(animal_acc_info$local_identifier, "cat"),]

test <- move2::movebank_download_study(
  study_id = 352166254, 
  sensor_type_id = "acceleration",
  individual_id = 1332827274,
  timestamp_start = as.POSIXct("2020-02-04 15:56:31") - lubridate::days(50),
  timestamp_end = as.POSIXct("2020-02-04 15:56:31")
)


test
mt_time_column(test)

lags <- mt_time_lags(test)
units::units(lags)
rle(as.numeric(round(lags, digits = 0)))

multi_ind |> 
  filter_track_data(deployment_id == 1393966634 )


rle(as.numeric(mt_time_lags(test)))

test 



