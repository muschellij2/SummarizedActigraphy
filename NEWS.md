# SummarizedActigraphy 0.8.0

* Ported `standardize_data`, `resample_accel_data`, and `resample_accel_data_to_time` from the `walking package.

# SummarizedActigraphy 0.6.1

* Added ability to read step counts from ActiLife CSV.

# SummarizedActigraphy 0.5.0

* Added `calibrate` and `estimate_calibration_values` to work for `GGIR`.
* Added `ggir_process` so that we can actually use some `GGIR` functionality.

# SummarizedActigraphy 0.4.2

* Added `flag_*` functions from https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm#Quality_Assurance_&_Quality_Control.
* Added `ensure_all_time` flag for `calculate_measures`.


# SummarizedActigraphy 0.3.1

* Added `by_second` option for `fix_zeros`.

# SummarizedActigraphy 0.3.0

* Slot is now `data` not `data.out` in `read_actigraphy`.
* All things `epoch` changed to `unit`. 
* Added `collapse_daily_actigraphy` so that you can collapse daily data.

# SummarizedActigraphy 0.1.0

* Added `actigraph*`, where we currently have `eBayes` and `lmFit`.

* Added a `NEWS.md` file to track changes to the package.

* Added all the functions and the ability to download data.
