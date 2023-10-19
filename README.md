
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

6.  Prepare merged data to output. The output data is a `move2` location
    object comprising the entire content of the input data with the
    following add-ons:

    - *event data*: the list-column `acc_dt` comprising the downloaded
      and merged ACC events

    - *track data*: columns `acc_dwn_start_time`, `acc_dwn_end_time` and
      `acc_in_timespan` providing information about the ACC downloading
      step.

    - *object attribute*: `acc_merging_rule` specifying the chosen
      merging criteria - retrievable via
      `attr(output, "acc_merging_rule")`

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

### Input data

`move2` location object

### Output data

`move2` location object

### Artefacts

None

### Parameters

**Movebank username** (`usr`): character string, the Movebank username.
Default: `NULL`

**Movebank password** (`pwd`): character string, the Movebank password.
Default: `NULL`

### Most common errors

None yet, but most likely ones to occur are: - Miss-match in track names
between location and non-location data

<!-- *Please describe shortly what most common errors of the App can be, how they occur and best ways of solving them.* -->

### Null or error handling

**Movebank username** (`usr`) and **Movebank password** (`pwd`): If one
of the credentials are either NULL or connection to Movebank is voided
due to invalid log-in details, the app will return an error reminding
the user to provide valid credentials.

### Example usage of merged data
