# Fetch and Bind Acceleration to Locations

MoveApps

Github repository: <https://github.com/dmpstats/Fetch_and_Derive_Acceleration_in_Locations>

## Description

Retrieves available acceleration data for individuals present in the input
location data and binds acceleration measuremets to each observed location based
on recorded time.

## Documentation

Given the individuals present in the input location dataset, this App is tasked
with retrieving and binding available (non-location) acceleration data to
recorded locations. Binding is done at day-level and is time-based. Underlying
data processing is outlined in the following steps:

1. Extracts IDs and start-end timestamps of location events for individuals
present in input data;

2. Checks availability of accelerometer data for each individual and timeframe;

3. Downloads sought accelerometer data via move2::download_study_data;

4. Pre-processes acceleration data, including averaging out multiple
acceleration values recorded per single observation in some of the tags;

5. Binds acceleration data, consisting of measurements at 3 accelerometer axis,
to each location event in the input dataset based on individual, day and time of
the day. Binding currently uses a non-linear (GAM) acceleration-given-time
relationship fitted internally on-the-fly to the downloaded accelerometer dataset -
however, this step will be revised to favor a potentially more simplistic
approach (e.g. interpolation);

6. Outputs location data with associated acceleration measurements.


**Note**: This App is a workaround for the current restriction on MoveApps to
initiate workflows from only one of either location or non-location data.
MoveApps has plans to relax this restriction in the future (e.g. multiple data
inputs on Apps), so eventually this App will become deprecated.


### Input data

`move2` location object


### Output data

`move2` location object

### Artefacts

None


### Settings

**Movebank username** (`user`): character string, the Movebank username. Default: `NULL`

**Movebank password** (`pass`): character string, the Movebank password. Default: `NULL`


### Most common errors

None yet, but most likely ones to occur are:
- Miss-match in track names between location and non-location data

<!-- *Please describe shortly what most common errors of the App can be, how they occur and best ways of solving them.* -->

### Null or error handling

**Movebank username** (`user`) and **Movebank password** (`pass`): If one of the
credentials are either NULL or connection to Movebank is voided due to invalid
log-in details, the app will return an error reminding the user to provide valid
credentials.
