run_resample = function(
    timestamp, x, y, z,
    time_interp,
    orig_tz,
    method = c("linear", "constant",
               "fmm", "periodic", "natural", "monoH.FC", "hyman"),
    ...) {
  method = match.arg(method)
  func = switch(
    method,
    linear = stats::approx,
    constant = stats::approx,
    fmm = stats::spline,
    periodic = stats::spline,
    natural = stats::spline,
    monoH.FC = stats::spline,
    hyman = stats::spline)
  x_out = func(x = timestamp,
               xout = time_interp,
               method = method,
               y = x,
               ...)$y
  rm(x)
  y_out = func(x = timestamp,
               xout = time_interp,
               method = method,
               y = y,
               ...)$y
  rm(y)
  z_out = func(x = timestamp,
               xout = time_interp,
               method = method,
               y = z,
               ...)$y
  rm(z)
  rm(timestamp)


  time_interp = round(time_interp, 3)
  time_interp = as.POSIXct(
    time_interp,
    tz = orig_tz,
    origin = lubridate::origin)

  out = data.frame(
    time = time_interp,
    X = x_out,
    Y = y_out,
    Z = z_out
  )
  # this should allow us to match whatever standardize_data does
  out = standardize_data(data = out, subset = FALSE)
  out
}
#' Resample 3-axial input signal to a specific sampling rate
#'
#' @param data A `data.frame` with a column for time in `POSIXct` (usually
#' `time`), and `X`, `Y`, `Z`
#' @param sample_rate sampling frequency, coercible to an integer.
#' This is the sampling rate you're sampling the data *into*.
#' @param ... additional arguments to pass to [stats::approx()] or
#' [stats::spline]
#' @param method method for interpolation. Options are
#' `"linear"/"constant"`, which uses `stats::approx`, or one of
#' `"fmm", "periodic", "natural", "monoH.FC", "hyman"`, which uses
#' `stats::spline`
#' @return A `data.frame`/`tibble` of `time` and `X`, `Y`, `Z`.
#' @export
#'
#' @examples
#' options(digits.secs = 3)
#' gt3x_file = system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "SummarizedActigraphy")
#' x = read_actigraphy(gt3x_file)$data
#' res = resample_accel_data(data = x, sample_rate = 80)
#' res = resample_accel_data(data = x, sample_rate = 100)
#' res = resample_accel_data(data = x, sample_rate = 1)
#' res = resample_accel_data_to_time(
#'   data = x,
#'   times = lubridate::floor_date(x$time, unit = "1 sec"),
#' )
#' res_nat = resample_accel_data_to_time(
#'   data = x,
#'   times = lubridate::floor_date(x$time, unit = "1 sec"),
#'   method = "natural"
#' )
resample_accel_data = function(
    data,
    sample_rate,
    method = "linear",
    ...
) {
  assertthat::assert_that(
    assertthat::is.count(sample_rate)
  )
  sample_rate = as.integer(sample_rate)

  data = standardize_data(data)
  orig_tz = lubridate::tz(data$time)
  timestamp = as.numeric(data$time)
  x = data[["X"]]
  y = data[["Y"]]
  z = data[["Z"]]
  is_data_tibble = inherits(data, "tbl_df")
  rm(data)

  # Make it 0 to max so that no overflow issues with numerics
  time_interp = timestamp - timestamp[1]
  time_interp = seq(time_interp[1],
                    time_interp[length(time_interp)],
                    (1/sample_rate)
  )
  time_interp = time_interp + timestamp[1]

  out = run_resample(
    timestamp = timestamp,
    x = x,
    y = y,
    z = z,
    method = method,
    time_interp = time_interp,
    orig_tz = orig_tz,
    ...)
  if (is_data_tibble) {
    out = dplyr::as_tibble(out)
  }
  return(out)
}

#' @export
#' @param times a vector of `POSIXct` date/time values to interpolate
#' the data to
#' @rdname resample_accel_data
resample_accel_data_to_time = function(
    data,
    times,
    method = "linear",
    ...
) {

  data = standardize_data(data)
  orig_tz = lubridate::tz(data$time)
  time_tz = lubridate::tz(times)
  if (orig_tz != time_tz) {
    stop("Timezone in data times do not match timezone in time vector!")
  }
  timestamp = as.numeric(data$time)
  x = data[["X"]]
  y = data[["Y"]]
  z = data[["Z"]]
  is_data_tibble = inherits(data, "tbl_df")
  rm(data)

  time_interp = as.numeric(times)
  rm(times)

  out = run_resample(
    timestamp = timestamp,
    x = x,
    y = y,
    z = z,
    method = method,
    time_interp = time_interp,
    orig_tz = orig_tz,
    ...)
  if (is_data_tibble) {
    out = dplyr::as_tibble(out)
  }
  return(out)
}
