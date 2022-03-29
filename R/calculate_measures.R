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
#' @param calculate_ac Should Activity Counts from the \code{activityCounts}
#' package be calculated?
#' @param flag_data Should [SummarizedActigraphy::flag_qc()] be run?
#' It will be executed after \code{fix_zeros} before any measure
#' calculation
#' @param flags the flags to calculate,
#' passed to [SummarizedActigraphy::flag_qc()]
#' @param ensure_all_time if \code{TRUE}, then all times from the first to
#' last times will be in the output, even if data during that time was not
#' in the input
#' @param sample_rate Sample rate of the data, only used if
#' `calculcate_ac = TRUE`
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
  calculate_ac = TRUE,
  flag_data = TRUE,
  flags = NULL,
  ensure_all_time = TRUE,
  verbose = TRUE,
  sample_rate = NULL,
  ...) {


  if (calculate_ac && !requireNamespace("activityCounts", quietly = TRUE)) {
    stop("activityCounts package required for calculating AC")
  }

  if (calculate_mims && !requireNamespace("MIMSunit", quietly = TRUE)) {
    stop("MIMSunit package required for calculating MIMS")
  }

  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  if (calculate_mims || flag_data) {
    dynamic_range = get_dynamic_range(df, dynamic_range)
  }
  # keep flag here - so can calculate by epoch
  is_data_table = is_dt(df)
  df = ensure_header_timestamp(df)
  if (calculate_ac) {
    sample_rate = get_sample_rate(df, sample_rate = sample_rate)
  }
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
    df = flag_qc_all(df, dynamic_range = dynamic_range, verbose = verbose,
                     flags = flags)
    df$flags = rowSums(
      df %>%
        dplyr::select(dplyr::starts_with("flag_")) > 0
    )
    flags = calculate_flags(df, unit = unit)
    df = df %>%
      dplyr::select(-dplyr::starts_with("flag"))
  }
  transformations = get_transformations(df)

  if (verbose) {
    message("Calculating ai0")
  }
  res = calculate_ai(df, unit = unit, verbose = verbose > 1)
  if (verbose) {
    message("Calculating MAD")
  }
  mad = calculate_mad(df, unit = unit, verbose = verbose > 1)
  if (verbose) {
    message("Joining AI and MAD")
  }

  # ai0 is res
  res = dplyr::full_join(res, mad, by = "HEADER_TIME_STAMP")
  rm(mad)
  if (calculate_mims) {
    if (verbose) {
      message("Calculating MIMS")
    }
    mims = calculate_mims(df, unit = unit,
                          dynamic_range = dynamic_range,
                          ...)
  }

  if (calculate_ac) {
    if (verbose) {
      message("Calculating AC")
    }
    ac = calculate_ac(df, unit = unit,
                      sample_rate = sample_rate,
                      verbose = verbose)
    ac$X = ac$Y = ac$Z = NULL
    if (verbose) {
      message("Joining AC")
    }
    res = dplyr::full_join(res, ac, by = "HEADER_TIME_STAMP")
    rm(ac)
  }
  rm(df)

  if (calculate_mims) {
    if (verbose) {
      message("Joining MIMS")
    }
    res = dplyr::full_join(res, mims, by = "HEADER_TIME_STAMP")
  }
  if (flag_data) {
    if (verbose) {
      message("Joining flags")
    }
    res = dplyr::full_join(res, flags, by = "HEADER_TIME_STAMP")
  }
  res = join_all_time(res, unit, ensure_all_time)
  res = res %>%
    dplyr::rename(time = HEADER_TIME_STAMP)
  transforms = paste("aggregated_at_", paste(unit, collapse = "_"))
  transformations = c(transforms, transformations)
  res = set_transformations(res, transformations = transformations, add = FALSE)
  res
}

floor_sec = function(x) {
  if (lubridate::is.POSIXct(x)) {
    tz = lubridate::tz(x)
    x = as.numeric(x)
    x = floor(x)
    as.POSIXct(x, tz = tz, origin = lubridate::origin)
  } else {
    lubridate::floor_date(x, "1 sec")
  }
}

remake_dt = function(df, is_data_table = FALSE) {
  if (requireNamespace("data.table", quietly = TRUE) &&
      is_data_table) {
    df = data.table::as.data.table(df)
  }
  df
}
is_dt = function(x) {
  inherits(x, "data.table")
}

.datatable.aware=TRUE

#' @export
#' @rdname calculate_measures
calculate_ai = function(df, unit = "1 min", ensure_all_time = TRUE,
                        verbose = FALSE) {
  # globals workup
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  AI = NULL
  rm(list = c("AI"))

  is_data_table = is_dt(df)
  df = ensure_header_timestamp(df)
  if (is_data_table &&
      requireNamespace("data.table", quietly = TRUE)) {
    df = remake_dt(df, is_data_table = is_data_table)
    df = df[, HEADER_TIME_STAMP := floor_sec(HEADER_TIME_STAMP)]
    if (verbose) {
      message("Summarizing the variance")
    }
    df = df[, .(X = var(X, na.rm = TRUE),
                Y = var(Y, na.rm = TRUE),
                Z = var(Z, na.rm = TRUE)),
            by = .(HEADER_TIME_STAMP)]
    df$AI = sqrt(1/3 * (df$X + df$Y + df$Z))
    df = as.data.frame(df)[c("HEADER_TIME_STAMP", "AI")]
  } else {
    if (verbose) {
      message("Running floor_sec on time")
    }
    # need tz because converting to integer for speed
    df = df %>%
      dplyr::mutate(HEADER_TIME_STAMP = floor_sec(HEADER_TIME_STAMP))

    if (verbose) {
      message("Summarizing the variance")
    }
    df = df %>%
      dplyr::group_by(HEADER_TIME_STAMP) %>%
      dplyr::summarise(
        AI = var(X, na.rm = TRUE) +
          var(Y, na.rm = TRUE) +
          var(Z, na.rm = TRUE)
      )
    # removed this because running sqrt and / 3 in grouped setting
    # is inefficient - ungrouping below and mutate
    # dplyr::summarise(
    #   AI = sqrt(
    #     (
    #       var(X, na.rm = TRUE) +
    #         var(Y, na.rm = TRUE) +
    #         var(Z, na.rm = TRUE)
    #     ) / 3)
    # )
    if (verbose) {
      message("Calculating AI")
    }
    if (!is_data_table) {
      df = df %>%
        dplyr::ungroup()
    }
    df = df %>%
      dplyr::mutate(AI = sqrt(AI/3))
  }

  df = df %>%
    dplyr::mutate(
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      AI = sum(AI)
    )
  df = df %>%
    tibble::as_tibble()%>%
    dplyr::ungroup()
  df = join_all_time(df, unit, ensure_all_time)
  df = remake_dt(df, is_data_table = is_data_table)
  df
}

join_all_time = function(df, unit = "1 min", ensure_all_time) {
  if (ensure_all_time) {
    rtime = range(df$HEADER_TIME_STAMP)
    time_df = tibble::tibble(HEADER_TIME_STAMP = seq(rtime[1], rtime[2],
                                                     by = unit))
    df = dplyr::left_join(time_df, df, by = "HEADER_TIME_STAMP")
  }
  df
}
#' @export
#' @rdname calculate_measures
calculate_activity_index = calculate_ai

#' @export
#' @rdname calculate_measures
calculate_flags = function(df, unit = "1 min", ensure_all_time = TRUE) {
  time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  df = ensure_header_timestamp(df, subset = FALSE)
  if (!any(grepl("^flag", colnames(df)))) {
    stop("flag is not in the data, please run flag_qc")
  }

  df = df %>%
    dplyr::mutate(
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      dplyr::across(dplyr::starts_with("flag"), sum),
      n_samples_in_unit = dplyr::n()
    ) %>%
    ungroup()
  df = join_all_time(df, unit, ensure_all_time)
  df
}


#' @export
#' @rdname calculate_measures
calculate_n_idle = function(df, unit = "1 min", ensure_all_time = TRUE) {
  ENMO = time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time", "ENMO"))
  df = ensure_header_timestamp(df)

  df = fix_zeros(df, fill_in = FALSE, trim = FALSE)


  n_idle = r = all_zero = NULL
  rm(list= c("n_idle", "r", "all_zero"))
  df = df %>%
    dplyr::mutate(
      r = sqrt(X^2+Y^2+Z^2),
      # ENMO = r - 1,
      # ENMO = dplyr::if_else(ENMO < 0, 0, ENMO),
      all_zero = X == 0 & Y == 0 & Z == 0,
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      n_idle = sum(is.na(r) | all_zero)
    ) %>%
    dplyr::ungroup()
  df = join_all_time(df, unit, ensure_all_time)
  df
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
calculate_ai_defined = function(...) {
  AI_DEFINED = time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time", "AI_DEFINED"))
  out = calculate_mad(...)
  out %>%
    dplyr::select(HEADER_TIME_STAMP, AI_DEFINED)
}


#' @export
#' @rdname calculate_measures
calculate_mad = function(df, unit = "1 min", ensure_all_time = TRUE,
                         verbose = FALSE) {
  ENMO_t = time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  is_data_table = is_dt(df)
  df = ensure_header_timestamp(df)

  if (verbose) {
    message("Calculating r, ENMO, and flooring time")
  }
  if (is_data_table &&
      requireNamespace("data.table", quietly = TRUE)) {
    df = remake_dt(df, is_data_table = is_data_table)
    if (verbose) {
      message("Summarizing the variance")
    }
    df = df[, r := X^2 + Y^2 + Z^2]
    df = df[, HEADER_TIME_STAMP := lubridate::floor_date(
      HEADER_TIME_STAMP, unit)]

    df = df[, ENMO_t := r - 1]
    df = df[, ENMO_t := dplyr::if_else(ENMO_t < 0, 0, ENMO_t)]

    if (verbose) {
      message("Calculating all MAD measures")
    }
    df = df[, .(
      SD = sd(r, na.rm = TRUE),
      SD_t = sd(ENMO_t, na.rm = TRUE),
      AI_DEFINED = sqrt((
        var(X, na.rm = TRUE) +
          var(Y, na.rm = TRUE) +
          var(Z, na.rm = TRUE)) / 3),
      MAD = mean(abs(r - mean(r, na.rm = TRUE)), na.rm = TRUE),
      MEDAD = median(abs(r - mean(r, na.rm = TRUE)), na.rm = TRUE),
      mean_r = mean(r, na.rm = TRUE),
      ENMO_t = mean(ENMO_t, na.rm = TRUE)
    ), by = .(HEADER_TIME_STAMP)]
  } else {
    df = df %>%
      dplyr::mutate(
        r = sqrt(X^2+Y^2+Z^2),
        ENMO_t = r - 1,
        ENMO_t = dplyr::if_else(ENMO_t < 0, 0, ENMO_t),
        HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                  unit))

    if (verbose) {
      message("Calculating all MAD measures")
    }
    df = df %>%
      dplyr::group_by(HEADER_TIME_STAMP) %>%
      dplyr::summarise(
        SD = sd(r, na.rm = TRUE),
        SD_t = sd(ENMO_t, na.rm = TRUE),
        AI_DEFINED = sqrt((
          var(X, na.rm = TRUE) +
            var(Y, na.rm = TRUE) +
            var(Z, na.rm = TRUE)) / 3),
        MAD = mean(abs(r - mean(r, na.rm = TRUE)), na.rm = TRUE),
        MEDAD = median(abs(r - mean(r, na.rm = TRUE)), na.rm = TRUE),
        mean_r = mean(r, na.rm = TRUE),
        ENMO_t = mean(ENMO_t, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
  }
  df = df %>%
    tibble::as_tibble()%>%
    dplyr::ungroup()
  df = join_all_time(df, unit, ensure_all_time)
  df = remake_dt(df, is_data_table = is_data_table)
  df
}

#' @rdname calculate_measures
#' @export
get_sample_rate = function(df, sample_rate = NULL) {
  if (!is.null(sample_rate)) {
    return(sample_rate)
  }
  if (is.AccData(df)) {
    sample_rate = df$freq
  }
  if (is.null(sample_rate) || is.na(sample_rate)) {
    sample_rate = attr(df, "sample_rate")
  }
  if ((is.null(sample_rate) || is.na(sample_rate)) &&
      any(c("time", "HEADER_TIME_STAMP", "HEADER_TIMESTAMP") %in% colnames(df))) {
    warning("Guessing sample_rate from the data")
    time = df[["time"]]
    if (is.null(time)) {
      time = df[["HEADER_TIME_STAMP"]]
    }
    if (is.null(time)) {
      time = df[["HEADER_TIMESTAMP"]]
    }
    d = diff(time)
    units(d) = "secs"
    rm(list = "time")
    if (all(d > 1)) {
      # minute level data
      sample_rate = unique(1 / as.numeric(d))
    } else {
      sample_rate = unique(round(1 / as.numeric(d)))
    }
    stopifnot(length(sample_rate) == 1)
  }
  stopifnot(!is.null(sample_rate))
  return(sample_rate)
}

#' @export
#' @rdname calculate_measures
#' @param sample_rate sample rate of data, if not specified in header of object
#' @param allow_truncation truncate small values
calculate_auc = function(df, unit = "1 min",
                         sample_rate = NULL,
                         allow_truncation = FALSE,
                         ensure_all_time = TRUE,
                         verbose = TRUE
) {
  dtime = good = NULL
  rm(list = c("good", "dtime"))
  dtime = time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time", "dtime"))
  AUC_X = AUC_Y = AUC_Z = NULL
  rm(list= c("AUC_X", "AUC_Z", "AUC_Y"))
  df = ensure_header_timestamp(df)
  sample_rate = get_sample_rate(df, sample_rate)

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
  df = join_all_time(df, unit, ensure_all_time)
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
  ensure_all_time = TRUE,
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
    ensure_all_time = ensure_all_time,
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
  ensure_all_time = TRUE,
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
  df = MIMSunit::mims_unit(
    df,
    epoch = unit,
    dynamic_range = dynamic_range,
    ...)
  df = df %>% dplyr::mutate(
    HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                              unit = unit))
  df = join_all_time(df, unit, ensure_all_time)

  df

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
  transformations = get_transformations(df)
  HEADER_TIMESTAMP = time = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list = c("HEADER_TIMESTAMP", "HEADER_TIME_STAMP", "X", "Y", "Z", "r", "time"))
  serial_prefix = sample_rate = NULL
  dynamic_range = NULL
  if (is.AccData(df)) {
    hdr = df$header
    if (all(c("Field", "Value") %in% names(hdr))) {
      dynamic_range = c(
        hdr$Value[hdr$Field == "Acceleration Min"],
        hdr$Value[hdr$Field == "Acceleration Max"])
      dynamic_range = as.numeric(dynamic_range)
      serial_prefix = substr(hdr$Value[hdr$Field == "Serial Number"], 1, 3)
      if (length(serial_prefix) == 0) {
        serial_prefix = NULL
      }
    }
    if (length(dynamic_range) == 0) {
      dynamic_range = NULL
    }

    sample_rate = df$freq
    df = df$data
    attr(df, "dynamic_range") = dynamic_range
    if (is.null(attr(df, "serial_prefix"))) {
      attr(df, "serial_prefix") = serial_prefix
    }

  }
  serial_prefix = attr(df, "serial_prefix")
  dynamic_range = attr(df, "dynamic_range")

  if (is.null(sample_rate)) {
    sample_rate = attr(df, "sample_rate")
  }
  df = tibble::as_tibble(df)
  cn = colnames(df)
  if ("time" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    df = df %>%
      dplyr::rename(HEADER_TIME_STAMP = time)
  }
  if ("HEADER_TIMESTAMP" %in% cn && !"HEADER_TIME_STAMP" %in% cn) {
    df = df %>%
      dplyr::rename(HEADER_TIME_STAMP = HEADER_TIMESTAMP)
  }
  if (subset) {
    df = df %>%
      dplyr::select(HEADER_TIME_STAMP, X, Y, Z)
  }
  stopifnot("HEADER_TIME_STAMP" %in% colnames(df))
  attr(df, "sample_rate") = sample_rate
  attr(df, "dynamic_range") = dynamic_range
  attr(df, "serial_prefix") = serial_prefix
  df = set_transformations(df, transformations = transformations, add = FALSE)
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
    if (is.null(dynamic_range)) {
      if (length(hdr$accrange) > 0) {
        arange = try({
          unique(abs(as.numeric(hdr$accrange)))
        }, silent = TRUE)
        if (!inherits(arange, "try-error")) {
          dynamic_range = c(-arange, arange)
        }
      }
    }

    # allows for actilife headers to extract it correctly
    if (is.null(dynamic_range) &&
        is.null(attr(df$data, "dynamic_range"))) {
      hdr = df$original_header
      dynamic_range = get_dynamic_range_actilife_header(hdr)
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

get_dynamic_range_actilife_header = function(header) {
  if (is.null(header)) {
    return(NULL)
  }
  if (length(header) > 0) {
    header = paste(header, collapse = " ")
  }
  hdr = strsplit(header, "---")[[1]]
  hdr = trimws(hdr)
  hdr = gsub("-", "", hdr)
  hdr = hdr[ !hdr %in% ""]
  hdr = trimws(hdr)
  hdr = hdr[ grepl("Serial", hdr)]
  ACTIGRAPH_SERIALNUM_PATTERN <- paste0(
    ".*Serial\\s*Number:",
    "\\s*([A-Za-z0-9]+)\\s*Start\\s*Time.*")
  sn = sub(ACTIGRAPH_SERIALNUM_PATTERN, "\\1", hdr)
  sn = trimws(sn)
  if (nchar(sn) > 20) {
    warning("Serial number does not seem to be parsed correctly, ",
            "dynamic range may be wrong")
  }
  at <- substr(sn, 1, 3)
  gr <- switch(at, MAT = "3", CLE = "6", MOS = "8", TAS = "8", NULL)
  if (grepl("IMU", hdr[[1]])) {
    gr <- "16"
  }
  gr = as.numeric(gr)
  gr = c(-gr, gr)
  if (length(gr) == 0) {
    gr = NULL
  }
  gr
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
