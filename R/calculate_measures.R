#' Calculate Summary Measures from Raw Accelerometer Data
#'
#' @param df An object with columns `X`, `Y`, and `Z` or an
#' object of class `AccData`
#' @param epoch length of time to calculate measures over.  a character string
#' specifying a time unit or a multiple of a unit to be rounded to.
#' Valid base units are `second`, `minute`, `hour`, `day`, `week`, `month`,
#' `bimonth`, `quarter`, `season`, `halfyear`, and `year`.
#' Arbitrary unique English abbreviations as in the \code{\link{period}}
#' constructor are allowed.
#' @param dynamic_range Dynamic range of the device, in gravity units
#' @param verbose print diagnostic messages
#' @param fix_zeros Should \code{\link{fix_zeros}} be run before calculating
#' the measures?
#' @param fill_in if \code{fix_zeros = TRUE}, should the zeros be
#' filled in with the last
#' @param trim if \code{fix_zeros = TRUE},
#' should the time course be trimmed for zero values at
#' the beginning and the end of the time course?
#' observation carried forward?
#' @param calculate_mims Should MIMS units be calculated?
#' @param ... additional arguments to pass to [MIMSunit::mims_unit]
#'
#' @return A data set with the calculated features
#' @export
calculate_measures = function(
  df, epoch = "1 min",
  fix_zeros = TRUE,
  fill_in = TRUE,
  trim = FALSE,
  dynamic_range = c(-6, 6),
  calculate_mims = TRUE,
  verbose = TRUE,
  ...) {

  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  df = ensure_header_timestamp(df)
  if (fix_zeros) {
    if (verbose) {
      message(
        paste0("Fixing Zeros with fix_zeros")
      )
    }
    df = fix_zeros(df, fill_in = fill_in, trim = trim)
  }
  if (verbose) {
    message("Calculating ai0")
  }
  ai0 = calculate_ai(df, epoch = epoch)
  if (verbose) {
    message("Calculating MAD")
  }
  mad = calculate_mad(df, epoch = epoch)
  if (calculate_mims) {
    if (verbose) {
      message("Calculating MIMS")
    }
    mims = calculate_mims(df, epoch = epoch,
                          dynamic_range = dynamic_range,
                          ...)
  }

  if (verbose) {
    message("Joining AI and MAD")
  }
  res = dplyr::full_join(ai0, mad)
  if (calculate_mims) {
    if (verbose) {
      message("Joining MIMS")
    }
    res = dplyr::full_join(res, mims)
  }
  res = res %>%
    dplyr::rename(time = HEADER_TIME_STAMP)
  res
}

#' @export
#' @rdname calculate_measures
calculate_ai = function(df, epoch = "1 min") {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  df = ensure_header_timestamp(df)

  AI = NULL
  rm(list= c("AI"))
  sec_df = df %>%
    dplyr::mutate(
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                "1 sec")) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      AI = sqrt((
        var(X, na.rm = TRUE) +
          var(Y, na.rm = TRUE) +
          var(Z, na.rm = TRUE)) / 3),
    )
  sec_df %>%
    dplyr::mutate(
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                epoch)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      AI = sum(AI)
    )
}

#' @export
#' @rdname calculate_measures
calculate_n_idle = function(df, epoch = "1 min") {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  df = ensure_header_timestamp(df)

  df = fix_zeros(df, fill_in = FALSE, trim = FALSE)


  n_idle = r = all_zero = NULL
  rm(list= c("n_idle", "r", "all_zero"))
  df %>%
    dplyr::mutate(
      r = sqrt(X^2+Y^2+Z^2),
      all_zero = X == 0 & Y == 0 & Z == 0,
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                epoch)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      n_idle = sum(is.na(r) | all_zero)
    )
}

#' @export
#' @rdname calculate_measures
calculate_mad = function(df, epoch = "1 min") {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  df = ensure_header_timestamp(df)

  df %>%
    dplyr::mutate(
      r = sqrt(X^2+Y^2+Z^2),
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                epoch)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      SD = sd(r, na.rm = TRUE),
      MAD = mean(abs(r - mean(r, na.rm = TRUE)), na.rm = TRUE),
      MEDAD = median(abs(r - mean(r, na.rm = TRUE)), na.rm = TRUE),
      mean_r = mean(r, na.rm = TRUE)
    )
}

#' @export
#' @rdname calculate_measures
calculate_mims = function(
  df,
  epoch = "1 min",
  dynamic_range = c(-6, 6),
  ...) {
  df = ensure_header_timestamp(df)
  if (!requireNamespace("MIMSunit", quietly = TRUE)) {
    stop("MIMSunit package required for calculating MIMS")
  }
  MIMSunit::mims_unit(
    df,
    epoch = epoch,
    dynamic_range = dynamic_range,
    ...)
}

ensure_header_timestamp = function(df) {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  if (is.AccData(df)) {
    df = df$data
  }
  cn = colnames(df)
  if ("time" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    df = df %>%
      dplyr::rename(HEADER_TIME_STAMP = time)
  }
  df = df %>%
    dplyr::select(HEADER_TIME_STAMP, X, Y, Z)
  df
}

