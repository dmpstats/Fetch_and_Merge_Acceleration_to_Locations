
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

- Downloaded data is reshaped to return bursts of ACC values nested in a
  list-column named `acc_bursts`. Therefore, each row of standardized
  ACC represents the event at which a burst of ACC values (per active
  axis) was recorded, with the timestamp column specifying the starting
  time of the burst.

- The type (raw or non-raw) and tag origin (eobs or non-eobs) of ACC
  samples are also added as data attributes.

- ACC data can optionally be thinned down to only retain a single ACC
  burst event within a given time interval via the parameter
  `acc_timefilter` (details in the Settings section below). This feature
  can be useful to avoid long computations, and potentially lack of
  memory issues, when processing high-frequency ACC data (e.g. 1 burst
  every couple of seconds).

5.  Merge ACC events to location events of the same animal/track based
    on recording timestamps and according to a choice of two merging
    rules:

- `"latest"`: ACC events are allocated to the latest location event
  preceding the starting time at which ACC bursts were recorded

- `"nearest"`: ACC events are allocated to the closest-in-time location
  event, which can occur either before or after the ACC sampling
  starting time.

<!-- #This means -->

<!-- 

more than one burst/event associated with wich location event

5. Binds acceleration data, consisting of measurements at 3 accelerometer axis,
to each location event in the input dataset based on individual, day and time of
the day. 

 -->

6.  Outputs location data with merged ACC data with relevant info on
    merging

<!-- 
Describe added columns in both event and track data. Also mention the ACC track data stored as an object attribute

including object attributes

-->

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
