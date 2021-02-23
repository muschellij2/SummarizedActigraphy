#' Calculate Summary Measures from Raw Accelerometer Data
#'
#' @param df An object with columns `X`, `Y`, and `Z` or an
#' object of class `AccData`
#' @param unit length of time to calculate measures over.  a character string
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
#' @param by_second Should the last observation carried forward be done
#' only within the same second?
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
  df, unit = "1 min",
  fix_zeros = TRUE,
  fill_in = TRUE,
  by_second = FALSE,
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
    df = fix_zeros(df, fill_in = fill_in, trim = trim, by_second = by_second)
  }
  if (verbose) {
    message("Calculating ai0")
  }
  ai0 = calculate_ai(df, unit = unit)
  if (verbose) {
    message("Calculating MAD")
  }
  mad = calculate_mad(df, unit = unit)
  if (calculate_mims) {
    if (verbose) {
      message("Calculating MIMS")
    }
    mims = calculate_mims(df, unit = unit,
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
calculate_ai = function(df, unit = "1 min") {
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
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      AI = sum(AI)
    )
}

#' @export
#' @rdname calculate_measures
calculate_n_idle = function(df, unit = "1 min") {
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
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      n_idle = sum(is.na(r) | all_zero)
    )
}

#' @export
#' @rdname calculate_measures
calculate_mad = function(df, unit = "1 min") {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  df = ensure_header_timestamp(df)

  df %>%
    dplyr::mutate(
      r = sqrt(X^2+Y^2+Z^2),
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)) %>%
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
calculate_auc = function(df, unit = "1 min") {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  AUC_X = AUC_Y = AUC_Z = NULL
  rm(list= c("AUC_X", "AUC_Z", "AUC_Y"))
  df = ensure_header_timestamp(df)

  df %>%
    dplyr::mutate(
      X = abs(X),
      Y = abs(Y),
      Z = abs(Z),
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      AUC_X = sum(X),
      AUC_Y = sum(Y),
      AUC_Z = sum(Z)) %>%
    ungroup() %>%
    dplyr::mutate(
      AUC = AUC_X + AUC_Y + AUC_Z
    )
}


#' @export
#' @rdname calculate_measures
calculate_mims = function(
  df,
  unit = "1 min",
  dynamic_range = c(-6, 6),
  ...) {
  HEADER_TIME_STAMP = NULL
  rm(list= "HEADER_TIME_STAMP")
  check = check_dynamic_range(df, dynamic_range = dynamic_range)
  if (!check) {
    msg = "Dynamic range does not cover all the data in df, please check data"
    warning(msg)
  }
  df = ensure_header_timestamp(df)
  if (!requireNamespace("MIMSunit", quietly = TRUE)) {
    stop("MIMSunit package required for calculating MIMS")
  }
  out = MIMSunit::mims_unit(
    df,
    epoch = unit,
    dynamic_range = dynamic_range,
    ...)
  out = out %>% dplyr::mutate(
    HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                              unit = unit))
  out

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

check_dynamic_range = function(df, dynamic_range = c(-6, 6)) {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  hdr = NULL
  if (is.AccData(df)) {
    hdr = df$header
    if (is.null(dynamic_range)) {
      dynamic_range = c(hdr$Value[hdr$Field== "Acceleration Min"],
                        hdr$Value[hdr$Field== "Acceleration Max"])
      dynamic_range = as.numeric(dynamic_range)
      if (length(dynamic_range) == 0) {
        dynamic_range = NULL
      }
    }
    df = df$data
  }
  stopifnot(length(dynamic_range) == 2,
            is.numeric(dynamic_range))

  cn = colnames(df)
  if ("time" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    df = df %>%
      dplyr::rename(HEADER_TIME_STAMP = time)
  }
  df = df %>%
    dplyr::select(HEADER_TIME_STAMP, X, Y, Z)
  r = range(df[SummarizedActigraphy::xyz], na.rm = TRUE)
  all(r >= dynamic_range[1] & r <= dynamic_range[2])
}

