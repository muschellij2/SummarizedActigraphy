
<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/muschellij2/SummarizedActigraphy.svg?branch=master)](https://travis-ci.com/muschellij2/SummarizedActigraphy)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/SummarizedActigraphy?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/SummarizedActigraphy)
[![R build
status](https://github.com/muschellij2/SummarizedActigraphy/workflows/R-CMD-check/badge.svg)](https://github.com/muschellij2/SummarizedActigraphy/actions)
[![Codecov test
coverage](https://codecov.io/gh/muschellij2/SummarizedActigraphy/branch/master/graph/badge.svg)](https://codecov.io/gh/muschellij2/SummarizedActigraphy?branch=master)
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
#> Input is a .gt3x file, unzipping to a temporary location first...
#> Unzipping gt3x data to /var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T//RtmpanwnsY
#> 1/1
#> Unzipping /var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T//RtmpanwnsY/GT3X+ (01 day).gt3x
#>  === info.txt, activity.bin, lux.bin extracted to /var/folders/1s/wrtqcpxn685_zk570bnx9_rr0000gr/T//RtmpanwnsY/GT3X+(01day)
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
#> Parsing GT3X data via CPP.. expected sample size: 2700000
#> Using NHANES-GT3X format - older format
#> Sample size: 2700000
#> Scaling...
#> Done (in 0.588511943817139 seconds)
#> Joining, by = "HEADER_TIME_STAMP"
#> Getting the First Day
#> Summarizing Data
result
#> # A tsibble: 1,440 x 11 [1m]
#>    time   AI_mean AI_median SD_mean SD_median MAD_mean MAD_median MEDAD_mean
#>    <time>   <dbl>     <dbl>   <dbl>     <dbl>    <dbl>      <dbl>      <dbl>
#>  1 00'00"  0.266      0.177 0.00646   0.00507  0.00373    0.00317    0.00221
#>  2 01'00"  0.323      0.230 0.0150    0.00644  0.00578    0.00448    0.00271
#>  3 02'00"  0.144      0.142 0.00402   0.00447  0.00252    0.00276    0.00132
#>  4 03'00"  0.947      0.643 0.0310    0.0145   0.0108     0.00640    0.00516
#>  5 04'00"  1.11       1.48  0.0324    0.0224   0.0130     0.0120     0.00428
#>  6 05'00"  0.536      0.214 0.00836   0.00615  0.00498    0.00294    0.00274
#>  7 06'00"  0.0921     0.122 0.00511   0.00641  0.00441    0.00460    0.00372
#>  8 07'00"  0.179      0.130 0.00979   0.00834  0.00587    0.00566    0.00380
#>  9 08'00"  0.572      0.139 0.0289    0.00503  0.00769    0.00301    0.00240
#> 10 09'00"  0.387      0.427 0.0167    0.0208   0.00580    0.00593    0.00254
#> # … with 1,430 more rows, and 3 more variables: MEDAD_median <dbl>,
#> #   mean_r_mean <dbl>, mean_r_median <dbl>
```

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
#> Joining, by = "time"
head(x)
#> # A tibble: 6 x 2
#>   time   AI_mean
#>   <time>   <dbl>
#> 1 00'00"   0.266
#> 2 01'00"   0.323
#> 3 02'00"   0.144
#> 4 03'00"   0.947
#> 5 04'00"   1.11 
#> 6 05'00"   0.536
x1440 = tidyr::spread(x, time, value = measure)
x1440
#> # A tibble: 1 x 1,440
#>   `00:00:00` `00:01:00` `00:02:00` `00:03:00` `00:04:00` `00:05:00` `00:06:00`
#>        <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#> 1      0.266      0.323      0.144      0.947       1.11      0.536     0.0921
#> # … with 1,433 more variables: `00:07:00` <dbl>, `00:08:00` <dbl>,
#> #   `00:09:00` <dbl>, `00:10:00` <dbl>, `00:11:00` <dbl>, `00:12:00` <dbl>,
#> #   `00:13:00` <dbl>, `00:14:00` <dbl>, `00:15:00` <dbl>, `00:16:00` <dbl>,
#> #   `00:17:00` <dbl>, `00:18:00` <dbl>, `00:19:00` <dbl>, `00:20:00` <dbl>,
#> #   `00:21:00` <dbl>, `00:22:00` <dbl>, `00:23:00` <dbl>, `00:24:00` <dbl>,
#> #   `00:25:00` <dbl>, `00:26:00` <dbl>, `00:27:00` <dbl>, `00:28:00` <dbl>,
#> #   `00:29:00` <dbl>, `00:30:00` <dbl>, `00:31:00` <dbl>, `00:32:00` <dbl>,
#> #   `00:33:00` <dbl>, `00:34:00` <dbl>, `00:35:00` <dbl>, `00:36:00` <dbl>,
#> #   `00:37:00` <dbl>, `00:38:00` <dbl>, `00:39:00` <dbl>, `00:40:00` <dbl>,
#> #   `00:41:00` <dbl>, `00:42:00` <dbl>, `00:43:00` <dbl>, `00:44:00` <dbl>,
#> #   `00:45:00` <dbl>, `00:46:00` <dbl>, `00:47:00` <dbl>, `00:48:00` <dbl>,
#> #   `00:49:00` <dbl>, `00:50:00` <dbl>, `00:51:00` <dbl>, `00:52:00` <dbl>,
#> #   `00:53:00` <dbl>, `00:54:00` <dbl>, `00:55:00` <dbl>, `00:56:00` <dbl>,
#> #   `00:57:00` <dbl>, `00:58:00` <dbl>, `00:59:00` <dbl>, `01:00:00` <dbl>,
#> #   `01:01:00` <dbl>, `01:02:00` <dbl>, `01:03:00` <dbl>, `01:04:00` <dbl>,
#> #   `01:05:00` <dbl>, `01:06:00` <dbl>, `01:07:00` <dbl>, `01:08:00` <dbl>,
#> #   `01:09:00` <dbl>, `01:10:00` <dbl>, `01:11:00` <dbl>, `01:12:00` <dbl>,
#> #   `01:13:00` <dbl>, `01:14:00` <dbl>, `01:15:00` <dbl>, `01:16:00` <dbl>,
#> #   `01:17:00` <dbl>, `01:18:00` <dbl>, `01:19:00` <dbl>, `01:20:00` <dbl>,
#> #   `01:21:00` <dbl>, `01:22:00` <dbl>, `01:23:00` <dbl>, `01:24:00` <dbl>,
#> #   `01:25:00` <dbl>, `01:26:00` <dbl>, `01:27:00` <dbl>, `01:28:00` <dbl>,
#> #   `01:29:00` <dbl>, `01:30:00` <dbl>, `01:31:00` <dbl>, `01:32:00` <dbl>,
#> #   `01:33:00` <dbl>, `01:34:00` <dbl>, `01:35:00` <dbl>, `01:36:00` <dbl>,
#> #   `01:37:00` <dbl>, `01:38:00` <dbl>, `01:39:00` <dbl>, `01:40:00` <dbl>,
#> #   `01:41:00` <dbl>, `01:42:00` <dbl>, `01:43:00` <dbl>, `01:44:00` <dbl>,
#> #   `01:45:00` <dbl>, `01:46:00` <dbl>, …
```
