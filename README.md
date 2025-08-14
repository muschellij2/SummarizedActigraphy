
<!-- badges: start -->

[![R-CMD-check](https://github.com/muschellij2/SummarizedActigraphy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/muschellij2/SummarizedActigraphy/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/muschellij2/SummarizedActigraphy/branch/master/graph/badge.svg)](https://codecov.io/gh/muschellij2/SummarizedActigraphy?branch=master)
[![Codecov test
coverage](https://codecov.io/gh/muschellij2/SummarizedActigraphy/graph/badge.svg)](https://app.codecov.io/gh/muschellij2/SummarizedActigraphy)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# SummarizedActigraphy Package:

The goal of `SummarizedActigraphy` is to provide functions for reading
Actigraphy data and turn it into `SummarizedExperiment`s.

## Installation

You can install `SummarizedActigraphy` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("muschellij2/SummarizedActigraphy")
```

## Reading in some data

``` r
library(SummarizedActigraphy)
url = paste0("https://github.com/THLfi/read.gt3x/files/",
             "3522749/GT3X%2B.01.day.gt3x.zip")
destfile = tempfile(fileext = ".zip")
dl = utils::download.file(url, destfile = destfile)
gt3x_file = utils::unzip(destfile, exdir = tempdir())
gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
path = gt3x_file
result = summarize_actigraphy(path)
#> Running Daily Actigraphy
#> Getting the First Day
#> Summarizing Data
result
#> # A tsibble: 1,440 x 13 [1m]
#>    time   AI_mean AI_median SD_mean SD_median MAD_mean MAD_median MEDAD_mean
#>    <time>   <dbl>     <dbl>   <dbl>     <dbl>    <dbl>      <dbl>      <dbl>
#>  1 00'00"  0.168     0.168  0.00613   0.00613  0.00394    0.00394    0.00291
#>  2 01'00"  0.0501    0.0501 0.00514   0.00514  0.00393    0.00393    0.00276
#>  3 02'00"  0         0      0         0        0          0          0      
#>  4 03'00"  0         0      0         0        0          0          0      
#>  5 04'00"  0         0      0         0        0          0          0      
#>  6 05'00"  0         0      0         0        0          0          0      
#>  7 06'00"  0.0802    0.0802 0.00375   0.00375  0.00308    0.00308    0.00216
#>  8 07'00"  0.0271    0.0271 0.00298   0.00298  0.00206    0.00206    0.00124
#>  9 08'00"  0.0865    0.0865 0.00411   0.00411  0.00355    0.00355    0.00452
#> 10 09'00"  0.0543    0.0543 0.00230   0.00230  0.00197    0.00197    0.00142
#> # ℹ 1,430 more rows
#> # ℹ 5 more variables: MEDAD_median <dbl>, ENMO_t_mean <dbl>,
#> #   ENMO_t_median <dbl>, mean_r_mean <dbl>, mean_r_median <dbl>
```

## Reading in GT3X files

In GT3X files, you must be aware of [Idle Sleep
Mode](https://actigraphcorp.my.site.com/support/s/article/Idle-Sleep-Mode-Explained),
which saves power on the device, but the device essentially stops
recording. In most cases, you still want a “full” time series without
missing elements. To read in `gt3x` files, we use
`read.gt3x::read.gt3x`. By default `read.gt3x::read.gt3x`, that data
isn’t in the output data set. If you use the `imputeZeros = TRUE`
argument, then they are there, but all 0s, which doesn’t make sense (no
gravity even!?).

In Actigraph’s ActiLife, the raw output data repeats the same value
before the device went into sleep mode. To mimic this behavior, the
`SummarizedActigraphy::fix_zeros` function sets the values in the rows
with all `0` to be `NA` (just for `X/Y/Z`, not `time`), and then uses
`zoo::na.locf` for last observation carried forward (LOCF). Aside: you
could also use `tidyr::fill(direction = "down")` for the tidyverse
folks.

In `SummarizedActigraphy::read_actigraphy`, we use
`read.gt3x::read.gt3x(asDataFrame = TRUE, imputeZeroes = TRUE)` as the
default.

See below as an example.

``` r
data = SummarizedActigraphy::read_actigraphy(path)
#> Input is a .gt3x file, unzipping to a temporary location first...
#> Unzipping gt3x data to /var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T//RtmpquRevB
#> 1/1
#> Unzipping /var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T//RtmpquRevB/GT3X+ (01 day).gt3x
#>  === info.txt, activity.bin, lux.bin extracted to /var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T//RtmpquRevB/GT3X+(01day)
#> GT3X information
#>  $ Serial Number     :"NEO1DXXXXXXXX"
#>  $ Device Type       :"GT3XPlus"
#>  $ Firmware          :"2.5.0"
#>  $ Battery Voltage   :"4.22"
#>  $ Sample Rate       :30
#>  $ Start Date        : POSIXct, format: "2012-06-27 10:54:00"
#>  $ Stop Date         : POSIXct, format: "2012-06-28 11:54:00"
#>  $ Download Date     : POSIXct, format: "2012-06-28 16:25:52"
#>  $ Board Revision    :"4"
#>  $ Unexpected Resets :"0"
#>  $ Sex               :"Male"
#>  $ Height            :"172.72"
#>  $ Mass              :"69.8532249799612"
#>  $ Age               :"43"
#>  $ Race              :"White / Caucasian"
#>  $ Limb              :"Ankle"
#>  $ Side              :"Left"
#>  $ Dominance         :"Non-Dominant"
#>  $ DateOfBirth       :"621132192000000000"
#>  $ Subject Name      :"GT3XPlus"
#>  $ Serial Prefix     :"NEO"
#>  $ Last Sample Time  : 'POSIXct' num(0) 
#>  - attr(*, "tzone")= chr "GMT"
#>  $ Acceleration Scale:341
#>  $ Acceleration Min  :"-6.0"
#>  $ Acceleration Max  :"6.0"
#> Parsing GT3X data via CPP.. expected sample size: 2700000
#> Using NHANES-GT3X format - older format
#> Sample size: 2700000
#> Scaling...
#> Done (in 0.675724983215332 seconds)
df = data$data
df = tibble::as_tibble(df)
all_zero = df$X == 0 & df$Y == 0 & df$Z == 0
df[all_zero, ] %>% 
  dplyr::mutate(time = lubridate::floor_date(time, "1 sec")) %>% 
  dplyr::distinct()
#> # A tibble: 3,341 × 4
#>    time                    X     Y     Z
#>    <dttm>              <dbl> <dbl> <dbl>
#>  1 2012-06-27 10:54:00     0     0     0
#>  2 2012-06-27 10:54:01     0     0     0
#>  3 2012-06-27 10:54:02     0     0     0
#>  4 2012-06-27 10:54:03     0     0     0
#>  5 2012-06-27 10:54:04     0     0     0
#>  6 2012-06-27 11:27:40     0     0     0
#>  7 2012-06-27 11:27:41     0     0     0
#>  8 2012-06-27 11:27:42     0     0     0
#>  9 2012-06-27 11:27:43     0     0     0
#> 10 2012-06-27 11:27:44     0     0     0
#> # ℹ 3,331 more rows
df = SummarizedActigraphy::fix_zeros(df)
df[all_zero, ] %>% 
  dplyr::mutate(time = lubridate::floor_date(time, "1 sec")) %>% 
  dplyr::distinct()
#> # A tibble: 3,341 × 4
#>    time                     X      Y     Z
#>    <dttm>               <dbl>  <dbl> <dbl>
#>  1 2012-06-27 10:54:00  0      0      0   
#>  2 2012-06-27 10:54:01  0      0      0   
#>  3 2012-06-27 10:54:02  0      0      0   
#>  4 2012-06-27 10:54:03  0      0      0   
#>  5 2012-06-27 10:54:04  0      0      0   
#>  6 2012-06-27 11:27:40 -0.123 -0.029 -1.04
#>  7 2012-06-27 11:27:41 -0.123 -0.029 -1.04
#>  8 2012-06-27 11:27:42 -0.123 -0.029 -1.04
#>  9 2012-06-27 11:27:43 -0.123 -0.029 -1.04
#> 10 2012-06-27 11:27:44 -0.123 -0.029 -1.04
#> # ℹ 3,331 more rows
```

Note well, the `fix_zeros` does not do anything with data that is all
zero in the beginning of a time series (as there is no observation to
carry forward). This behavior should mimic ActiLife to our knowledge and
that data is likely to be discarded regardless.

## Converting to wide 1440 format

``` r
hms_times = structure(seq(0, 86340, by = 60), class = c("hms", "difftime"),
                      units = "secs")
hms_times = tibble::tibble(time = hms_times)
measure = "AI_mean"
tmeasure = c("time", measure)
x = result[, tmeasure, drop = FALSE]
x = tibble::as_tibble(x)
x = dplyr::left_join(hms_times, x)
#> Joining with `by = join_by(time)`
head(x)
#> # A tibble: 6 × 2
#>   time   AI_mean
#>   <time>   <dbl>
#> 1 00'00"  0.168 
#> 2 01'00"  0.0501
#> 3 02'00"  0     
#> 4 03'00"  0     
#> 5 04'00"  0     
#> 6 05'00"  0
x1440 = tidyr::spread(x, time, value = measure)
x1440
#> # A tibble: 1 × 1,440
#>   `00:00:00` `00:01:00` `00:02:00` `00:03:00` `00:04:00` `00:05:00` `00:06:00`
#>        <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#> 1      0.168     0.0501          0          0          0          0     0.0802
#> # ℹ 1,433 more variables: `00:07:00` <dbl>, `00:08:00` <dbl>, `00:09:00` <dbl>,
#> #   `00:10:00` <dbl>, `00:11:00` <dbl>, `00:12:00` <dbl>, `00:13:00` <dbl>,
#> #   `00:14:00` <dbl>, `00:15:00` <dbl>, `00:16:00` <dbl>, `00:17:00` <dbl>,
#> #   `00:18:00` <dbl>, `00:19:00` <dbl>, `00:20:00` <dbl>, `00:21:00` <dbl>,
#> #   `00:22:00` <dbl>, `00:23:00` <dbl>, `00:24:00` <dbl>, `00:25:00` <dbl>,
#> #   `00:26:00` <dbl>, `00:27:00` <dbl>, `00:28:00` <dbl>, `00:29:00` <dbl>,
#> #   `00:30:00` <dbl>, `00:31:00` <dbl>, `00:32:00` <dbl>, `00:33:00` <dbl>, …
```
