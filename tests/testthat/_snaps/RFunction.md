# Acc data is merged to location data as expected

    Code
      tidyr::unnest(dplyr::mutate(merged_nearest, acc_min_time = purrr::map(acc_dt, ~ min(.$timestamp)), acc_max_time = purrr::map(acc_dt, ~ max(.$timestamp))), c(acc_min_time, acc_max_time))
    Output
      Simple feature collection with 39 features and 15 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -1.157729 ymin: -2.271925 xmax: 2.310297 ymax: 2.581959
      CRS:           NA
      # A tibble: 39 × 16
         sensor_type_id individual_local_identifier data_decoding_software eobs_acceleration_sampling_frequency_per_axis eobs_key_bin_checksum eobs_start_timestamp import_marked_outlier timestamp          
                <int64> <fct>                       <fct>                                                           [Hz]               <int64> <dttm>               <lgl>                 <dttm>             
       1        2365683 HL462 (3020)                <NA>                                                            10.5            3111998064 2014-03-17 17:05:00  NA                    2014-03-17 17:05:00
       2        2365683 HL462 (3020)                <NA>                                                            10.5            2990451605 2014-03-17 18:00:22  NA                    2014-03-17 18:00:22
       3        2365683 HL462 (3020)                <NA>                                                            10.5            1335860193 2014-03-17 19:00:21  NA                    2014-03-17 19:00:21
       4        2365683 HL462 (3020)                <NA>                                                            10.5             568490435 2014-03-17 20:00:23  NA                    2014-03-17 20:00:23
       5        2365683 HL462 (3020)                <NA>                                                            10.5            1324957751 2014-03-18 02:00:42  NA                    2014-03-18 02:00:42
       6        2365683 HL462 (3020)                <NA>                                                            10.5            2927769524 2014-03-18 03:00:42  NA                    2014-03-18 03:00:42
       7        2365683 HL462 (3020)                <NA>                                                            10.5            1236635572 2014-03-18 04:00:18  NA                    2014-03-18 04:00:18
       8        2365683 HL462 (3020)                <NA>                                                            10.5            2613185565 2014-03-18 05:00:08  NA                    2014-03-18 05:00:08
       9        2365683 HL462 (3020)                <NA>                                                            10.5            1247233951 2014-03-18 06:00:10  NA                    2014-03-18 06:00:10
      10        2365683 HL462 (3020)                <NA>                                                            10.5             742144296 2014-03-18 07:00:07  NA                    2014-03-18 07:00:07
          event_id visible                geometry is_acc_raw is_acc_eobs acc_dt            acc_min_time        acc_max_time       
           <int64> <lgl>                   <POINT> <lgl>      <lgl>       <list>            <dttm>              <dttm>             
       1 295227834 TRUE      (-0.5021924 0.970202) TRUE       TRUE        <move2 [6 × 14]>  2014-03-17 17:05:00 2014-03-17 17:30:00
       2 295227845 TRUE     (0.1315312 -0.1016292) TRUE       TRUE        <move2 [12 × 14]> 2014-03-17 17:35:00 2014-03-17 18:30:02
       3 295227857 TRUE     (-0.07891709 1.403203) TRUE       TRUE        <move2 [12 × 14]> 2014-03-17 18:35:02 2014-03-17 19:30:02
       4 295227869 TRUE      (0.8867848 -1.776776) TRUE       TRUE        <move2 [6 × 14]>  2014-03-17 19:35:02 2014-03-17 20:00:23
       5 295227870 TRUE      (0.1169713 0.6228674) TRUE       TRUE        <move2 [7 × 14]>  2014-03-18 02:00:42 2014-03-18 02:30:02
       6 295227882 TRUE     (0.3186301 -0.5222834) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 02:35:02 2014-03-18 03:30:01
       7 295227894 TRUE      (-0.5817907 1.322231) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 03:35:01 2014-03-18 04:30:01
       8 295227906 TRUE     (0.7145327 -0.3634403) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 04:35:01 2014-03-18 05:30:02
       9 295227918 TRUE      (-0.8252594 1.319066) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 05:35:02 2014-03-18 06:30:00
      10 295227930 TRUE    (-0.3598621 0.04377907) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 06:35:00 2014-03-18 07:30:00
      # ℹ 29 more rows

---

    Code
      tidyr::unnest(dplyr::mutate(merged_latest, acc_min_time = purrr::map(acc_dt, ~ min(.$timestamp)), acc_max_time = purrr::map(acc_dt, ~ max(.$timestamp))), c(acc_min_time, acc_max_time))
    Output
      Simple feature collection with 39 features and 15 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -1.157729 ymin: -2.271925 xmax: 2.310297 ymax: 2.581959
      CRS:           NA
      # A tibble: 39 × 16
         sensor_type_id individual_local_identifier data_decoding_software eobs_acceleration_sampling_frequency_per_axis eobs_key_bin_checksum eobs_start_timestamp import_marked_outlier timestamp          
                <int64> <fct>                       <fct>                                                           [Hz]               <int64> <dttm>               <lgl>                 <dttm>             
       1        2365683 HL462 (3020)                <NA>                                                            10.5            3111998064 2014-03-17 17:05:00  NA                    2014-03-17 17:05:00
       2        2365683 HL462 (3020)                <NA>                                                            10.5            2990451605 2014-03-17 18:00:22  NA                    2014-03-17 18:00:22
       3        2365683 HL462 (3020)                <NA>                                                            10.5            1335860193 2014-03-17 19:00:21  NA                    2014-03-17 19:00:21
       4        2365683 HL462 (3020)                <NA>                                                            10.5             568490435 2014-03-17 20:00:23  NA                    2014-03-17 20:00:23
       5        2365683 HL462 (3020)                <NA>                                                            10.5            1324957751 2014-03-18 02:00:42  NA                    2014-03-18 02:00:42
       6        2365683 HL462 (3020)                <NA>                                                            10.5            2927769524 2014-03-18 03:00:42  NA                    2014-03-18 03:00:42
       7        2365683 HL462 (3020)                <NA>                                                            10.5            1236635572 2014-03-18 04:00:18  NA                    2014-03-18 04:00:18
       8        2365683 HL462 (3020)                <NA>                                                            10.5            2613185565 2014-03-18 05:00:08  NA                    2014-03-18 05:00:08
       9        2365683 HL462 (3020)                <NA>                                                            10.5            1247233951 2014-03-18 06:00:10  NA                    2014-03-18 06:00:10
      10        2365683 HL462 (3020)                <NA>                                                            10.5             742144296 2014-03-18 07:00:07  NA                    2014-03-18 07:00:07
          event_id visible                geometry is_acc_raw is_acc_eobs acc_dt            acc_min_time        acc_max_time       
           <int64> <lgl>                   <POINT> <lgl>      <lgl>       <list>            <dttm>              <dttm>             
       1 295227834 TRUE      (-0.5021924 0.970202) TRUE       TRUE        <move2 [11 × 14]> 2014-03-17 17:05:00 2014-03-17 17:55:00
       2 295227845 TRUE     (0.1315312 -0.1016292) TRUE       TRUE        <move2 [12 × 14]> 2014-03-17 18:00:22 2014-03-17 18:55:01
       3 295227857 TRUE     (-0.07891709 1.403203) TRUE       TRUE        <move2 [12 × 14]> 2014-03-17 19:00:21 2014-03-17 19:55:01
       4 295227869 TRUE      (0.8867848 -1.776776) TRUE       TRUE        <move2 [1 × 14]>  2014-03-17 20:00:23 2014-03-17 20:00:23
       5 295227870 TRUE      (0.1169713 0.6228674) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 02:00:42 2014-03-18 02:55:02
       6 295227882 TRUE     (0.3186301 -0.5222834) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 03:00:42 2014-03-18 03:55:02
       7 295227894 TRUE      (-0.5817907 1.322231) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 04:00:18 2014-03-18 04:55:02
       8 295227906 TRUE     (0.7145327 -0.3634403) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 05:00:08 2014-03-18 05:55:02
       9 295227918 TRUE      (-0.8252594 1.319066) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 06:00:10 2014-03-18 06:55:00
      10 295227930 TRUE    (-0.3598621 0.04377907) TRUE       TRUE        <move2 [12 × 14]> 2014-03-18 07:00:07 2014-03-18 07:55:00
      # ℹ 29 more rows

---

    Code
      merge_acc_to_loc(acc[c(20:40, 70:90), ], loc, time_col = "timestamp", merging_rule = "latest")
    Output
      A <move2> object containing 1 track consisting of:
      Simple feature collection with 39 features and 13 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -1.157729 ymin: -2.271925 xmax: 2.310297 ymax: 2.581959
      CRS:           NA
      # A tibble: 39 × 14
         sensor_type_id individual_local_identifier data_decoding_software eobs_acceleration_sampling_frequency_per_axis eobs_key_bin_checksum eobs_start_timestamp import_marked_outlier timestamp          
       *        <int64> <fct>                       <fct>                                                           [Hz]               <int64> <dttm>               <lgl>                 <dttm>             
       1        2365683 HL462 (3020)                <NA>                                                            10.5            3111998064 2014-03-17 17:05:00  NA                    2014-03-17 17:05:00
       2        2365683 HL462 (3020)                <NA>                                                            10.5            2990451605 2014-03-17 18:00:22  NA                    2014-03-17 18:00:22
       3        2365683 HL462 (3020)                <NA>                                                            10.5            1335860193 2014-03-17 19:00:21  NA                    2014-03-17 19:00:21
       4        2365683 HL462 (3020)                <NA>                                                            10.5             568490435 2014-03-17 20:00:23  NA                    2014-03-17 20:00:23
       5        2365683 HL462 (3020)                <NA>                                                            10.5            1324957751 2014-03-18 02:00:42  NA                    2014-03-18 02:00:42
       6        2365683 HL462 (3020)                <NA>                                                            10.5            2927769524 2014-03-18 03:00:42  NA                    2014-03-18 03:00:42
       7        2365683 HL462 (3020)                <NA>                                                            10.5            1236635572 2014-03-18 04:00:18  NA                    2014-03-18 04:00:18
       8        2365683 HL462 (3020)                <NA>                                                            10.5            2613185565 2014-03-18 05:00:08  NA                    2014-03-18 05:00:08
       9        2365683 HL462 (3020)                <NA>                                                            10.5            1247233951 2014-03-18 06:00:10  NA                    2014-03-18 06:00:10
      10        2365683 HL462 (3020)                <NA>                                                            10.5             742144296 2014-03-18 07:00:07  NA                    2014-03-18 07:00:07
          event_id visible                geometry is_acc_raw is_acc_eobs acc_dt           
       *   <int64> <lgl>                   <POINT> <lgl>      <lgl>       <list>           
       1 295227834 TRUE      (-0.5021924 0.970202) TRUE       TRUE        <NULL>           
       2 295227845 TRUE     (0.1315312 -0.1016292) TRUE       TRUE        <move2 [4 × 14]> 
       3 295227857 TRUE     (-0.07891709 1.403203) TRUE       TRUE        <move2 [12 × 14]>
       4 295227869 TRUE      (0.8867848 -1.776776) TRUE       TRUE        <move2 [1 × 14]> 
       5 295227870 TRUE      (0.1169713 0.6228674) TRUE       TRUE        <move2 [4 × 14]> 
       6 295227882 TRUE     (0.3186301 -0.5222834) TRUE       TRUE        <NULL>           
       7 295227894 TRUE      (-0.5817907 1.322231) TRUE       TRUE        <move2 [3 × 14]> 
       8 295227906 TRUE     (0.7145327 -0.3634403) TRUE       TRUE        <move2 [12 × 14]>
       9 295227918 TRUE      (-0.8252594 1.319066) TRUE       TRUE        <move2 [6 × 14]> 
      10 295227930 TRUE    (-0.3598621 0.04377907) TRUE       TRUE        <NULL>           
      # ℹ 29 more rows
      Track features:
      # A tibble: 1 × 48
        deployment_id   tag_id individual_id deploy_off_timestamp deploy_on_timestamp deployment_local_identifier sensor_type_ids  capture_location deploy_on_location deploy_off_location
              <int64>  <int64>       <int64> <dttm>               <dttm>              <fct>                       <chr>                 <POINT [°]>        <POINT [°]>         <POINT [°]>
      1      10792927 10600886      10792910 2014-03-20 00:00:00  2013-07-17 00:00:00 HL462-3020                  acceleration,gps            EMPTY              EMPTY               EMPTY
        individual_comments                      individual_local_identifier ring_id   taxon_canonical_name individual_number_of_deployments mortality_location beacon_frequency tag_comments       
        <chr>                                    <fct>                       <fct>     <fct>                                           <int>        <POINT [°]>            [MHz] <chr>              
      1 Loburg, Loburg, sibling of HL460 & HL461 HL462 (3020)                DEH HL461 Ciconia ciconia                                     1              EMPTY             868. Pinger 18:00 - 6:00
        tag_local_identifier manufacturer_name tag_number_of_deployments study_id acknowledgements                                                                  
        <fct>                <fct>                                 <int>  <int64> <chr>                                                                             
      1 3020                 e-obs                                     1 10449318 This study links to the DIP study and is a collaboration with HUJ and Uni Potsdam.
        citation                                                                                                                                                                                               
        <chr>                                                                                                                                                                                                  
      1 "Data from study \"LifeTrack White Stork Loburg\" in www.movebank.org by Max Planck Institute of Animal Behavior (Radolfzell, Germany), Hebrew University of Jerusalem (Israel), University of Potsdam…
        grants_used                                                              has_quota i_am_owner is_test license_type name                         study_number_of_deployments number_of_individuals
        <chr>                                                                    <lgl>     <lgl>      <lgl>   <fct>        <fct>                                              <int>               [count]
      1 "German-Israeli Project Cooperation\n(DIP) by German Science Foundation" TRUE      FALSE      FALSE   CC_0         LifeTrack White Stork Loburg                          41                    41
        number_of_tags principal_investigator_name study_objective                                                                                                  study_type suspend_license_terms
               [count] <chr>                       <chr>                                                                                                            <fct>      <lgl>                
      1            182 martin (Martin Wikelski)    Determining migration routes of white storks in Eurasia. Lifetime tracking of storks. Involvement of the public. research   TRUE                 
        i_can_see_data there_are_data_which_i_cannot_see i_have_download_access i_am_collaborator study_permission timestamp_first_deployed_location timestamp_last_deployed_location
        <lgl>          <lgl>                             <lgl>                  <lgl>             <fct>            <dttm>                            <dttm>                          
      1 TRUE           FALSE                             TRUE                   FALSE             na               2013-07-03 09:10:56               2023-09-07 16:00:56             
        number_of_deployed_locations taxon_ids                    contact_person_name             main_location
                             [count] <chr>                        <fct>                             <POINT [°]>
      1                      4305481 Ciconia ciconia,Homo sapiens myotis (Wolfgang Fiedler) (12.08496 52.07782)

# time-points are correclty merged to a reference timeline

    Code
      data.frame(timeline = tmln[out_nearest$tmln_idx], tmpoints = tmpts[out_nearest$tmpt_idx])
    Output
                    timeline            tmpoints
      1  2022-01-01 00:00:00 2022-01-01 00:00:01
      2  2022-01-01 00:00:20 2022-01-01 00:00:11
      3  2022-01-01 00:00:20 2022-01-01 00:00:21
      4  2022-01-01 00:00:40 2022-01-01 00:00:31
      5  2022-01-01 00:00:40 2022-01-01 00:00:41
      6  2022-01-01 00:01:00 2022-01-01 00:00:51
      7  2022-01-01 00:01:00 2022-01-01 00:01:01
      8  2022-01-01 00:01:20 2022-01-01 00:01:11
      9  2022-01-01 00:01:20 2022-01-01 00:01:21
      10 2022-01-01 00:01:40 2022-01-01 00:01:31
      11 2022-01-01 00:01:40 2022-01-01 00:01:41
      12 2022-01-01 00:02:00 2022-01-01 00:01:51

---

    Code
      data.frame(timeline = tmln[out_latest$tmln_idx], tmpoints = tmpts[out_latest$tmpt_idx])
    Output
                    timeline            tmpoints
      1  2022-01-01 00:00:00 2022-01-01 00:00:01
      2  2022-01-01 00:00:00 2022-01-01 00:00:11
      3  2022-01-01 00:00:20 2022-01-01 00:00:21
      4  2022-01-01 00:00:20 2022-01-01 00:00:31
      5  2022-01-01 00:00:40 2022-01-01 00:00:41
      6  2022-01-01 00:00:40 2022-01-01 00:00:51
      7  2022-01-01 00:01:00 2022-01-01 00:01:01
      8  2022-01-01 00:01:00 2022-01-01 00:01:11
      9  2022-01-01 00:01:20 2022-01-01 00:01:21
      10 2022-01-01 00:01:20 2022-01-01 00:01:31
      11 2022-01-01 00:01:40 2022-01-01 00:01:41
      12 2022-01-01 00:01:40 2022-01-01 00:01:51

