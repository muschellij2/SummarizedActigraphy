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
#' @param flag_data Should [SummarizedActigraphy::flag_qc()] be run?
#' It will be executed after \code{fix_zeros} before any measure
#' calculation
#' @param ... additional arguments to pass to [MIMSunit::mims_unit]
#'
#' @return A data set with the calculated features
#' @export
#' @examples
#' file = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
#' package = "SummarizedActigraphy")
#' res = read_actigraphy(file)
#' measures = calculate_measures(res, dynamic_range = NULL,
#' calculate_mims = FALSE)
#' auc = calculate_auc(res)
#' \donttest{
#' mims = calculate_mims(res, dynamic_range = NULL)
#' }
#'
calculate_measures = function(
  df, unit = "1 min",
  fix_zeros = TRUE,
  fill_in = TRUE,
  by_second = FALSE,
  trim = FALSE,
  dynamic_range = NULL,
  calculate_mims = TRUE,
  flag_data = TRUE,
  verbose = TRUE,
  ...) {

  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  if (calculate_mims || flag_data) {
    dynamic_range = get_dynamic_range(df, dynamic_range)
  }
  # keep flag here - so can calculate by epoch
  df = ensure_header_timestamp(df)
  # or do flag_qc(verbose) if flag-dat = TRUE
  if (fix_zeros) {
    if (verbose) {
      message(
        paste0("Fixing Zeros with fix_zeros")
      )
    }
    df = fix_zeros(df, fill_in = fill_in, trim = trim, by_second = by_second)
  }
  if (flag_data) {
    if (verbose) {
      message("Flagging data")
    }
    df = flag_qc_all(df, dynamic_range = dynamic_range, verbose = verbose)
    flags = calculate_flags(df, unit = unit)
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

  rm(df)
  res = dplyr::full_join(ai0, mad)
  if (calculate_mims) {
    if (verbose) {
      message("Joining MIMS")
    }
    res = dplyr::full_join(res, mims)
  }
  if (flag_data) {
    if (verbose) {
      message("Joining flags")
    }
    res = dplyr::full_join(res, flags)
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
          var(Z, na.rm = TRUE)) / 3)
    )
  sec_df %>%
    dplyr::ungroup() %>%
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
calculate_flags = function(df, unit = "1 min") {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  df = ensure_header_timestamp(df, subset = FALSE)
  if (!any(grepl("^flag", colnames(df)))) {
    stop("flag is not in the data, please run flag_qc")
  }

  AI = NULL
  rm(list= c("AI"))
  df = df %>%
    dplyr::mutate(
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with("flag"), sum)
      ) %>%
    ungroup()
  df
}


#' @export
#' @rdname calculate_measures
calculate_n_idle = function(df, unit = "1 min") {
  ENMO = time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time", "ENMO"))
  df = ensure_header_timestamp(df)

  df = fix_zeros(df, fill_in = FALSE, trim = FALSE)


  n_idle = r = all_zero = NULL
  rm(list= c("n_idle", "r", "all_zero"))
  df %>%
    dplyr::mutate(
      r = sqrt(X^2+Y^2+Z^2),
      ENMO = r - 1,
      ENMO = dplyr::if_else(ENMO < 0, 0, ENMO),
      all_zero = X == 0 & Y == 0 & Z == 0,
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      n_idle = sum(is.na(r) | all_zero)
    ) %>%
    dplyr::ungroup()
}

#' @export
#' @rdname calculate_measures
calculate_enmo = function(...) {
  ENMO_t = time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  out = calculate_mad(...)
  out %>%
    dplyr::select(HEADER_TIME_STAMP, ENMO_t)
}

#' @export
#' @rdname calculate_measures
calculate_mad = function(df, unit = "1 min") {
  ENMO_t = time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  df = ensure_header_timestamp(df)

  df %>%
    dplyr::mutate(
      r = sqrt(X^2+Y^2+Z^2),
      ENMO_t = r - 1,
      ENMO_t = dplyr::if_else(ENMO_t < 0, 0, ENMO_t),
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      SD = sd(r, na.rm = TRUE),
      MAD = mean(abs(r - mean(r, na.rm = TRUE)), na.rm = TRUE),
      MEDAD = median(abs(r - mean(r, na.rm = TRUE)), na.rm = TRUE),
      mean_r = mean(r, na.rm = TRUE),
      ENMO_t = mean(ENMO_t, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}


#' @export
#' @rdname calculate_measures
#' @param sample_rate sample rate of data, if not specified in header of object
#' @param allow_truncation truncate small values
calculate_auc = function(df, unit = "1 min",
                         sample_rate = NULL,
                         allow_truncation = FALSE,
                         verbose = TRUE
) {
  dtime = good = NULL
  rm(list = c("good", "dtime"))
  dtime = time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time", "dtime"))
  AUC_X = AUC_Y = AUC_Z = NULL
  rm(list= c("AUC_X", "AUC_Z", "AUC_Y"))
  df = ensure_header_timestamp(df)
  if (is.null(sample_rate)) {
    sample_rate = attr(df, "sample_rate")
  }

  n_total = n_in_interval(unit, sample_rate)
  max_values <- 16 * n_total

  if (verbose) {
    message("Absolute values")
  }
  df = df %>%
    dplyr::mutate(
      X = abs(X),
      Y = abs(Y),
      Z = abs(Z))
  if (verbose) {
    message("Calculting trapezoids")
  }
  df = df %>%
    # trapezoidal
    dplyr::mutate(
      dtime = difftime(HEADER_TIME_STAMP, dplyr::lag(HEADER_TIME_STAMP, n = 1),
                       units = "secs"),
      dtime = as.numeric(dtime),
      X = (X + dplyr::lag(X, n = 1)) / 2 * dtime,
      Y = (Y + dplyr::lag(Y, n = 1)) / 2 * dtime,
      Z = (Z + dplyr::lag(Z, n = 1)) / 2 * dtime
    ) %>%
    dplyr::select(-dtime)
  if (verbose) {
    message("Replacing first value as NA")
  }
  replace_first_na = function(x) {
    x[1] = NA
    x
  }
  df = df %>%
    dplyr::mutate(
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)
    ) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    # trapezoidal
    dplyr::mutate(
      X = replace_first_na(X),
      Y = replace_first_na(Y),
      Z = replace_first_na(Z)
    )
  df = df %>%
    # ungroup for speed
    dplyr::ungroup() %>%
    dplyr::mutate(
      good = !(is.na(X) | is.na(Y) | is.na(Z))
    ) %>%
    dplyr::group_by(HEADER_TIME_STAMP)

  if (verbose) {
    message("Calculating AUCs")
  }
  df = df %>%
    dplyr::summarise(
      good = sum(good),
      AUC_X = sum(X, na.rm = TRUE),
      AUC_Y = sum(Y, na.rm = TRUE),
      AUC_Z = sum(Z, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(good = good >= (0.9 * n_total))
  df = df %>%
    dplyr::mutate(
      AUC_X = ifelse(good, AUC_X, NA),
      AUC_Y = ifelse(good, AUC_Y, NA),
      AUC_Z = ifelse(good, AUC_Z, NA)
    )
  if (allow_truncation) {
    if (verbose) {
      message("Truncating Small AUCs")
    }
    minimum = 1e-04 * n_total
    df = df %>%
      dplyr::mutate(
        AUC_X = ifelse(AUC_X <= minimum, 0, AUC_X),
        AUC_Y = ifelse(AUC_Y <= minimum, 0, AUC_Y),
        AUC_Z = ifelse(AUC_Z <= minimum, 0, AUC_Z)
      )
    df = df %>%
      dplyr::mutate(
        AUC_X = ifelse(AUC_X < 0 | AUC_X >= max_values, -1, AUC_X),
        AUC_Y = ifelse(AUC_Y < 0 | AUC_Y >= max_values, -1, AUC_Y),
        AUC_Z = ifelse(AUC_Z < 0 | AUC_Z >= max_values, -1, AUC_Z)
      )
  }
  df = df %>%
    dplyr::mutate(
      AUC = AUC_X + AUC_Y + AUC_Z
    ) %>%
    dplyr::select(-good)
  df
}

#' @export
#' @rdname calculate_measures
calculate_fast_mims = function(
  df,
  unit = "1 min",
  dynamic_range = NULL,
  sample_rate = NULL,
  allow_truncation = TRUE,
  verbose = TRUE,
  ...) {
  args = list(df,
              ...,
              verbose = verbose,
              dynamic_range = dynamic_range)
  output_mims_per_axis = FALSE
  if ("output_mims_per_axis" %in% names(args)) {
    output_mims_per_axis = args$output_mims_per_axis
    args$output_mims_per_axis = NULL
  }
  df = do.call(mims_default_processing, args = args)
  df = calculate_auc(
    df, unit = unit,
    sample_rate = sample_rate,
    allow_truncation = allow_truncation,
    verbose = verbose
  )
  colnames(df) = sub("AUC", "MIMS_UNIT", colnames(df))
  if (!output_mims_per_axis) {
    df = df[, c("HEADER_TIME_STAMP", "MIMS_UNIT")]
  }
  df
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

  dynamic_range = get_dynamic_range(df, dynamic_range = dynamic_range)
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

#' Ensure `HEADER_TIME_STAMP` is in
#'
#' @param df A data set of time and X/Y/Z
#' @param subset select only the columns of time and X/Y/Z
#'
#' @return A data set/`tibble`.  It does not return an `AccData` object
#' @export
#'
#' @examples
#' file = system.file("extdata",
#' "TAS1H30182785_2019-09-17.gt3x",
#' package = "SummarizedActigraphy")
#' res = read_actigraphy(file, verbose = FALSE)
#' df = ensure_header_timestamp(res)
ensure_header_timestamp = function(df, subset = TRUE) {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list = c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  sample_rate = NULL
  dynamic_range = NULL
  if (is.AccData(df)) {
    hdr = df$header
    if (all(c("Field", "Value") %in% names(hdr))) {
      dynamic_range = c(
        hdr$Value[hdr$Field == "Acceleration Min"],
        hdr$Value[hdr$Field == "Acceleration Max"])
      dynamic_range = as.numeric(dynamic_range)
    }
    if (length(dynamic_range) == 0) {
      dynamic_range = NULL
    }

    sample_rate = df$freq
    df = df$data
    attr(df, "dynamic_range") = dynamic_range
  }
  dynamic_range = attr(df, "dynamic_range")

  if (is.null(sample_rate)) {
    sample_rate = attr(df, "sample_rate")
  }
  cn = colnames(df)
  if ("time" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    df = df %>%
      dplyr::rename(HEADER_TIME_STAMP = time)
  }
  if (subset) {
    df = df %>%
      dplyr::select(HEADER_TIME_STAMP, X, Y, Z)
  }
  attr(df, "sample_rate") = sample_rate
  attr(df, "dynamic_range") = dynamic_range
  df
}

#' Get Dynamic Range
#'
#' @param df An \code{AccData} object from \code{\link{read_actigraphy}}
#' @param dynamic_range the dynamic range.  If this is not \code{NULL}, then
#' it will be guess from the header or the data
#'
#' @return A length-2 numeric vector, or the original dynamic range (no
#' checking done)
#' @export
get_dynamic_range = function(df, dynamic_range = NULL) {
  if (is.AccData(df)) {
    hdr = df$header
    if (is.null(dynamic_range)) {
      dynamic_range = c(hdr$Value[hdr$Field == "Acceleration Min"],
                        hdr$Value[hdr$Field == "Acceleration Max"])
      dynamic_range = as.numeric(dynamic_range)
      if (length(dynamic_range) == 0) {
        dynamic_range = NULL
      }
    }
    drange = attr(df, "dynamic_range")
    if (!is.null(drange)) {
      dynamic_range = drange
    }
    df = df$data
  }
  drange = attr(df, "dynamic_range")
  if (!is.null(drange)) {
    dynamic_range = drange
  }
  if (is.null(dynamic_range)) {
    warning("No dynamic range found in header, using data estimate")
    r = range(df[SummarizedActigraphy::xyz], na.rm = TRUE)
    r = max(abs(r))
    r = ceiling(r)
    dynamic_range = c(-r, r)
  }
  return(dynamic_range)
}

check_dynamic_range = function(df, dynamic_range = c(-6, 6)) {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  hdr = NULL

  dynamic_range = get_dynamic_range(df, dynamic_range)
  if (is.AccData(df)) {
    df = df$data
  }
  stopifnot(length(dynamic_range) == 2,
            is.numeric(dynamic_range))

  r = range(df[SummarizedActigraphy::xyz], na.rm = TRUE)
  all(r >= dynamic_range[1] & r <= dynamic_range[2])
}

n_in_interval = function(epoch, sample_rate = NULL) {
  stopifnot(!is.null(sample_rate))
  epoch = strsplit(epoch, " ")[[1]]
  if (length(epoch) == 1) {
    token = 1
  } else {
    stopifnot(length(epoch) == 2)
    token = as.numeric(epoch[1])
    epoch = epoch[2]
  }
  epoch = sub("s$", "", trimws(epoch))
  epoch = match.arg(epoch, c("seconds", "minutes", "hours", "days"))
  multiplier = switch(epoch,
                      seconds = 1,
                      minutes = 60,
                      hours = 60*60,
                      days = 60*60*24)
  n = token * sample_rate * multiplier
  return(n)
}
