
# Fetch and Merge Accelerometer data to Locations

MoveApps

Github repository:
<https://github.com/dmpstats/Fetch_and_Bind_Acceleration_to_Locations>

## Description

Downloads available accelerometer data for individuals and time window
comprised in the input location data and merges acceleration
measurements to each observed location based on recorded time.

## Documentation

Given the individuals and time-span of recorded events in the input
location data, this App is tasked with downloading and merging available
(non-location) acceleration data to recorded locations. Merging is
entirely time-based, and the underlying data processing can be outlined
as follows:

1.  Extract animal/track IDs and start-end timestamps of location events
    for individuals present in input data;

2.  Check user’s download permissions and availability of accelerometer
    (ACC) data for each individual;

3.  Download sought ACC data via `move2::download_study_data()`;

4.  Process ACC data into a standardized format. ACC samples can be
    recorded in Movebank in plain format (one ACC measurement on each
    active axis per row) or nested format (burst of consecutive AAC
    values stored per row). In this step:

    - Downloaded data is reshaped to return bursts of ACC values nested
      in a list-column named `acc_bursts`. Therefore, each row of
      standardized ACC represents an event at which a burst of ACC
      values (per active axis) was recorded, with the timestamp column
      specifying the starting time of the burst.

    - The type (raw or non-raw) and tag origin (eobs or non-eobs) of ACC
      samples are also added as data attributes.

    - ACC data can optionally be thinned down to only retain a single
      ACC burst event within a given time interval via the parameter
      `acc_timefilter` (details in the Settings section below). This
      feature can be useful to avoid long computations, and potentially
      lack of memory issues, when processing high-frequency ACC data
      (e.g. 1 burst every couple of seconds).

5.  Merge processed ACC events to location events of the same
    animal/track based on recording timestamps and according to a choice
    between two merging criteria:

    - `"latest"`: ACC events are allocated to the latest location event
      preceding the starting time at which ACC bursts were recorded

    - `"nearest"`: ACC events are allocated to the closest-in-time
      location event, which can occur either before or after the ACC
      sampling starting time.

    ACC events are joined to corresponding location events in a
    list-column named `acc_dt`. Data nesting is required here as,
    depending on the frequency of ACC events relative to location
    events, consecutive ACC events may be allocated to the same location
    event.

6.  Prepare merged data for output, where `output` is a `move2` location
    object containing the entire input data along with the following
    additions:

    - *event data*: the list-column `acc_dt` comprising the downloaded
      and merged ACC events

    - *track data*: columns `acc_dwn_start_time`, `acc_dwn_end_time` and
      `acc_in_timespan` providing information about the ACC downloading
      step.

    - *object attributes*:

      - `acc_merging_rule` specifying the selected merging criteria[^1]
      - `acc_track_data` providing the track data component of the
        merged ACC data

<br />

<div>

> **Note**
>
> This App is a workaround for the current restriction on MoveApps to
> initiate workflows from only one of either location or non-location
> data. MoveApps has plans to relax this restriction in the future
> (e.g. multiple data inputs on Apps), so this App may eventually become
> deprecated.

</div>

<br />

### Input data

`move2` location object

### Output data

`move2` location object (see details in point 6. above)

### Artefacts

`downloaded_acc_data.rds`: a `tibble` object with downloaded and
processed ACC data, with ACC events and track data nested by animal in
list-columns `acc_dt` and `acc_trk_dt`

### Parameters

**Movebank username** (`usr`): character string, the Movebank username.
Default: `NULL`.

**Movebank password** (`pwd`): character string, the Movebank password.
Default: `NULL`.

**Merging Criteria** (`merging_rule`): specify how downloaded
Accelerometer data is merged to location data. ACC events can allocated
to either:

- the latest recorded location available preceding sampling time
  (`"latest"`, the default), or
- the closest-in-time recorded location (`"nearest"`)

**Store ACC track information** (`store_acc_track_info`): check-box to
choose whether to store track data from merged ACC data as an attribute
of the output `move2` object. Default: `FALSE`.

**Filter downloaded ACC data by time interval** (`acc_timefilter`): an
integer defining the time interval, in minutes, for thinning the ACC
data. Must be between 0 (no filtering) and 30. Unit: `minutes`; default:
`0`

### Most common errors

None yet

<!-- *Please describe shortly what most common errors of the App can be, how they occur and best ways of solving them.* -->

### Null or error handling

**data**: the App will skip the merging step and return a modified
version of the input data with an empty column `acc_dt` if:

- Accelerometer data is not collected for any of the animals in the
  input data,
- the user does not have downloading permission for the study

An informative warning is also issued.

**Movebank username** (`usr`) and **Movebank password** (`pwd`): If one
of the credentials are either NULL or connection to Movebank is voided
due to invalid log-in details, the app will return an error reminding
the user to provide valid credentials.

**Filter downloaded ACC data by time interval** (`acc_timefilter`): The
App will throw an error if the specified interval is not between 0 and
30.

### Example usage of merged data

<!-- ```{r} -->
<!-- #| echo: false -->
<!-- #| include: false -->
<!-- library(move2) -->
<!-- source("RFunction.R") -->
<!-- # movebank credentials stored in HOME .Renviron file -->
<!-- usr <- Sys.getenv("vult_usr") -->
<!-- pwd <- Sys.getenv("vult_pwd") -->
<!-- source(file.path("src/common/logger.R")) -->
<!-- source(file.path("src/io/app_files.R")) -->
<!-- source(file.path("src/io/io_handler.R")) -->
<!-- input_dt <- readRDS("data/raw/gps_vult_nam.rds") |>  -->
<!--   filter_track_data(individual_local_identifier == "TO_6485") |>  -->
<!--   slice(1:10) -->
<!-- output_latest <- rFunction(input_dt, usr = usr, pwd = pwd, merging_rule = "latest") -->
<!-- ``` -->
<!-- ```{r} -->
<!-- output_latest$acc_dt[[1]] -->
<!-- ``` -->

[^1]: retrievable via `attr(output, "acc_merging_rule")`
