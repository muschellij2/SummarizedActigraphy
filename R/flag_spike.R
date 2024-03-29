#' Flag Spikes
#'
#' @param df A data set of actigraphy
#' @param spike_size size of "spike" - which is the absolute difference
#' in contiguous observations on a single axis
#'
#' @return A data set back
#' @export
#'
#' @source \url{https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm}
#'
#' @examples
#' file = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
#' package = "SummarizedActigraphy")
#' res = read_actigraphy(file)
#' res = flag_spike(res)
#' res = flag_interval_jump(res)
#' res = flag_spike_second(res)
#' res = flag_same_value(res)
#' res = flag_device_limit(res)
#' res = flag_all_zero(res)
#' res = flag_impossible(res)
flag_spike = function(df, spike_size = 11) {
  # from https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm
  # #1
  # Acceleration spikes recorded for the x-, y-, or z-axis at 80 Hz level:
  # A spike is recorded whenever the change between two adjacent samples of
  # raw accelerometer data in the same axis is greater than or equal to 11 g;
  X = Y = Z = NULL
  rm(list = c("X", "Y", "Z"))

  df = ensure_header_timestamp(df, subset = FALSE)
  df = df %>%
    dplyr::mutate(
      flag_spike =
        abs(c(0, diff(X))) >= spike_size |
        abs(c(0, diff(Y))) >= spike_size |
        abs(c(0, diff(Z))) >= spike_size
    )
  df
}


#' @export
#' @rdname flag_spike
#' @param verbose print diagnostic messages
flag_interval_jump = function(df, verbose = FALSE) {
  # from https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm
  # #8.
  # Interval jumps in the data on the x-, y-, or z-axis: There are at least
  # three g values that are occurring at least 10 times each in a second,
  # and those three most occurring g values are at least +/-0.5 g apart
  # from one another; and

  axis = time = X = Y = Z = NULL
  rm(list = c("X", "Y", "Z", "axis", "time"))

  df = ensure_header_timestamp(df, subset = FALSE)
  xdf = df

  value = n = NULL
  rm(list = c("n", "value"))
  HEADER_TIME_STAMP = NULL
  rm(list = c("HEADER_TIME_STAMP"))

  # group by second
  df = df %>%
    dplyr::mutate(HEADER_TIME_STAMP = floor_sec(HEADER_TIME_STAMP))
  if (verbose) {
    message("interval_jump: Data is floored")
  }

  # data is now long by axis
  df = tidy_axes(df)
  if (verbose) {
    message("interval_jump: Axes are long")
  }
  # sort it - not needed
  # df = df %>%
  #   dplyr::arrange(time, axis, value)
  # round data, in case it's not rounded
  # needed because count() is using unique values
  df = df %>%
    dplyr::mutate(value = round(value, 3) * 1000)
  if (verbose) {
    message("interval_jump: Rounded axes")
  }
  df = df %>%
    # just feel more comfortable counting integers rather than
    # floating point
    dplyr::group_by(time, axis) %>%
    dplyr::count(value) %>%
    dplyr::ungroup()

  # need values at least 10 times each in a second
  df = df %>%
    dplyr::filter(n >= 10)
  df = df %>%
    dplyr::mutate(value = value / 1000)
  df = df %>%
    dplyr::arrange(time, axis, dplyr::desc(n), value)
  # There are at least 3 g values that are occurring
  df = df %>%
    # need group for n() function
    dplyr::group_by(time, axis) %>%
    # keeping first 3 records
    # because docs say those 3 *most* occurring g values
    dplyr::filter(dplyr::n() >= 3) %>%
    # 2 filter as it was doing seq before
    dplyr::filter(dplyr::row_number() <= 3)
  if (verbose) {
    message("interval_jump: Filtered on 3")
  }
  # if (nrow(df) == 0) {
  #   xdf$flag_interval_jump = FALSE
  #   return(xdf)
  # }

  dvalue = NULL
  rm(list = "dvalue")
  # those 3 *most* occurring g values are at least 0.5 g apart from one another
  # data is arranged by n, not value, so need abs
  df = df %>%
    dplyr::group_by(time, axis) %>%
    dplyr::mutate(dvalue = abs(c(1, diff(value)))) %>%
    dplyr::ungroup()
  if (verbose) {
    message("interval_jump: diff value run")
  }
  # those 3 most occurring g values are at least 0.5 g apart from one another
  df = df %>%
    dplyr::mutate(dvalue = dvalue >= 0.5)

  flag_interval_jump = NULL
  rm(list = "flag_interval_jump")

  # must be true for all 3 values, so all(dvalue)
  # then just aggregate across axis
  df = df %>%
    dplyr::group_by(time, axis) %>%
    dplyr::summarise(flag_interval_jump = all(dvalue)) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(flag_interval_jump = any(flag_interval_jump)) %>%
    ungroup()
  if (verbose) {
    message("interval_jump: any interval flagged")
  }
  # only need these values
  df = df %>%
    dplyr::ungroup() %>%
    dplyr::select(time, flag_interval_jump)

  sample_rate = attr(xdf, "sample_rate")
  dynamic_range = attr(xdf, "dynamic_range")
  # merge with time - need original data
  xdf = xdf %>%
    dplyr::ungroup() %>%
    dplyr::mutate(time = floor_sec(HEADER_TIME_STAMP)) %>%
    dplyr::left_join(df, by = "time") %>%
    dplyr::mutate(
      flag_interval_jump = dplyr::if_else(is.na(flag_interval_jump), FALSE,
                                          flag_interval_jump)
    ) %>%
    dplyr::select(-time)
  rm(df)
  attr(xdf, "sample_rate") = sample_rate
  attr(xdf, "dynamic_range") = dynamic_range
  xdf
}

#' @export
#' @rdname flag_spike
#' @note `flag_spike` looks if 2 contiguous values, within each axis,
#' are larger than a absolute size (`11` gravity units).  The
#' `flag_spike_second` function groups the data by second, finds the
#' range of values, within each axis, and determines if this range is
#' greater than a specified size (`11` g).
flag_spike_second = function(df, spike_size = 11) {

  # from https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm
  # #7
  # Occurrence of spikes on the x-, y-, or z-axis in a 1-second
  # time period: within the 1-second windows of data containing 12 Hz
  # or more of fast (less than 100 milliseconds) and
  # large (greater than or equal to 11 g) changes in x-, y-, or z-axis values.
  # This test checks for unusually high frequencies of spike-like
  # behavior, but the spikes do not necessarily occur within
  # two adjacent samples, as they must for the acceleration spikes
  # check specified in above item #1;

  X = Y = Z = NULL
  rm(list = c("X", "Y", "Z"))

  df = ensure_header_timestamp(df, subset = FALSE)
  sample_rate = attr(df, "sample_rate")
  dynamic_range = attr(df, "dynamic_range")
  HEADER_TIME_STAMP = floor_HEADER_TIME_STAMP = NULL
  rm(list= c("floor_HEADER_TIME_STAMP", "HEADER_TIME_STAMP"))
  # check within 1 second window
  df = df %>%
    dplyr::mutate(floor_HEADER_TIME_STAMP = floor_sec(HEADER_TIME_STAMP)) %>%
    dplyr::group_by(floor_HEADER_TIME_STAMP) %>%
    # "spike" is really the range is greater than spike_size
    # don't need abs because range is always ordered
    dplyr::mutate(
      flag_spike_second =
        diff(range(X, na.rm = TRUE)) >= spike_size |
        diff(range(Y, na.rm = TRUE)) >= spike_size |
        diff(range(Z, na.rm = TRUE)) >= spike_size
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-floor_HEADER_TIME_STAMP)
  attr(df, "sample_rate") = sample_rate
  attr(df, "dynamic_range") = dynamic_range
  df
}

#' @export
#' @rdname flag_spike
#' @param dynamic_range dynamic range of the device, used to find the
#' device limit.
#' @param epsilon A small adjustment so that if values are within the
#' device limit, but minus epsilon, still flagged as hitting the limit.
#' For example, if `dynamic_range = c(-6, 6)` and `epsilon = 0.05`, then any
#' value <= `-5.95` or `>= 5.95` gravity units will be flagged
flag_device_limit = function(df, dynamic_range = NULL, epsilon = 0.05) {
  # from https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm
  # #2 and #3
  # Maximum gravity values on the x-, y-, or z-axis: Any value greater than
  # 5.95 g is deemed as near or above the maximum g value measured by the
  # device (i.e., +6g);

  dynamic_range = get_dynamic_range(df, dynamic_range)
  limit = max(abs(dynamic_range)) - epsilon

  X = Y = Z = NULL
  rm(list = c("X", "Y", "Z"))

  df = ensure_header_timestamp(df, subset = FALSE)
  df = df %>%
    dplyr::mutate(
      flag_device_limit =
        abs(X) >= limit |
        abs(Y) >= limit |
        abs(Z) >= limit
    )
  df
}

#' @rdname flag_spike
#' @export
flag_contiguous_device_limit = function(df, dynamic_range = NULL, epsilon = 0.05) {
  # from https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm
  # #1
  # Acceleration spikes recorded for the x-, y-, or z-axis at 80 Hz level:
  # A spike is recorded whenever the change between two adjacent samples of
  # raw accelerometer data in the same axis is greater than or equal to 11 g;
  X = Y = Z = NULL
  rm(list = c("X", "Y", "Z"))

  if (!"flag_device_limit" %in% colnames(df)) {
    df = flag_device_limit(df, dynamic_range = dynamic_range, epsilon = epsilon)
  } else {
    df = ensure_header_timestamp(df, subset = FALSE)
  }
  df = df %>%
    dplyr::mutate(
      flag_contiguous_device_limit = flag_device_limit &
        (dplyr::lag(flag_device_limit, default = FALSE) |
           dplyr::lead(flag_device_limit, default = FALSE)
        )
    )
  df
}



#' @export
#' @rdname flag_spike
#' @param min_length minimum length of the condition for contiguous
#' samples.  If `min_length = 3`, then at least 3 `TRUE`s in a row is required,
#' any stretches of single `TRUE` values or 2 `TRUE` followed by `FALSE`,
#' will be set to `FALSE`.
flag_same_value = function(df, min_length = 1) {
  # from https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm
  # #6
  # Contiguous adjacent identical non-zero values on the x-, y-, or z-axis:
  # all three axes are equal and contiguous but not zeros
  # (contiguous adjacent measures can continue between minutes, hours, or days);

  # NOTE! we do not check that these values are non-zero

  same_value = X = Y = Z = NULL
  rm(list = c("X", "Y", "Z", "same_value"))

  df = ensure_header_timestamp(df, subset = FALSE)
  sample_rate = attr(df, "sample_rate")
  dynamic_range = attr(df, "dynamic_range")
  df = df %>%
    dplyr::mutate(
      flag_same_value =
        c(1, diff(X)) == 0 &
        c(1, diff(Y)) == 0 &
        c(1, diff(Z)) == 0
    )
  df = df %>%
    dplyr::mutate(flag_same_value = mark_condition(flag_same_value,
                                                   min_length = min_length))
  attr(df, "sample_rate") = sample_rate
  attr(df, "dynamic_range")   = dynamic_range
  df
}

#' @export
#' @rdname flag_spike
flag_all_zero = function(df, min_length = 3) {
  # from https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm
  # #5
  # Contiguous adjacent zero values on the x-, y-, and z-axis: 3 or more
  # contiguous measures are zero on all three axes (contiguous measure
  # can continue between minutes, hours, or days);

  all_zero = X = Y = Z = NULL
  rm(list = c("X", "Y", "Z", "all_zero"))

  df = ensure_header_timestamp(df, subset = FALSE)
  sample_rate = attr(df, "sample_rate")
  dynamic_range = attr(df, "dynamic_range")
  df = df %>%
    dplyr::mutate(
      flag_all_zero = X == 0 & Y == 0 & Z == 0
    )
  df = df %>%
    dplyr::mutate(flag_all_zero = mark_condition(flag_all_zero,
                                                 min_length = min_length))
  attr(df, "sample_rate") = sample_rate
  attr(df, "dynamic_range")   = dynamic_range
  df
}


#' @export
#' @rdname flag_spike
flag_impossible = function(df, min_length = 6) {
  # from https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAXMIN_G.htm
  # #4
  # Contiguous impossible gravity measures such as,
  # zero gravity measures on the x-, y-, or z-axis:
  # Data with less than 0.01 g change per axis (indicating the device
  # is not moving) and the vector magnitude of the x-, y-, or
  # z-axis is greater than 1.25 g (indicating the gravitational
  # component exerted on a non-moving device is unrealistically large),
  # for at least six contiguous samples (i.e., 75 milliseconds);


  # Unclear what 0.01 g change per axis means
  # either the acceleration or the diff?!?
  abs_Z = abs_Y = abs_X = impossible = X = Y = Z = NULL

  rm(list = c("X", "Y", "Z", "impossible", "abs_X", "abs_Y", "abs_Z"))
  df = ensure_header_timestamp(df, subset = FALSE)
  df = df %>%
    dplyr::mutate(
      abs_X = abs(X),
      abs_Y = abs(Y),
      abs_Z = abs(Z),
      flag_impossible = abs_X <= 0.01 | abs_Y <= 0.01 | abs_Z <= 0.01,
      flag_impossible = flag_impossible &
        (abs_X >= 1.25 | abs_Y >= 1.25 | abs_Z >= 1.25)
    ) %>%
    dplyr::select(-abs_X, -abs_Y, -abs_Z)
  df = df %>%
    dplyr::mutate(flag_impossible = mark_condition(flag_impossible,
                                                   min_length = min_length))
  df
}

#' Flag Quality Control Values
#'
#' @param df A data set of actigraphy
#' @param verbose print diagnostic messages
#' @param dynamic_range dynamic range of the device, used to find the
#' device limit.
#' @param flags the flags to run for QC.  If you set this to \code{"all"},
#' then all flags are run, as the default.
#'
#' @return A data set with a `flags` column (`flag_qc`) or a number of
#' columns starting with `flag_*` (`flag_qc_all`)
#' @export
#'
#' @examples
#' file = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
#' package = "SummarizedActigraphy")
#' res = read_actigraphy(file)
#' out = flag_qc(res)
flag_qc = function(df, dynamic_range = NULL, verbose = TRUE,
                   flags = c("all", "spike", "interval_jump",
                             "spike_second",
                             "same_value", "device_limit",
                             "all_zero", "impossible")
){
  df = flag_qc_all(df, dynamic_range, verbose, flags = flags)
  transformations = get_transformations(df)
  transforms = NULL
  is_acc = is.AccData(df)
  if (is_acc) {
    hdr = df$header
    freq = df$freq
    filename = df$filename
    missingness = df$missingness
  }
  df = ensure_header_timestamp(df, subset = FALSE)
  df$flags = rowSums(
    df %>%
      dplyr::select(dplyr::starts_with("flag_")) > 0
  )
  df = df %>%
    dplyr::select(-dplyr::starts_with("flag_"))

  transforms = "flags_aggregated"
  transformations = c(transforms, transformations)
  df = set_transformations(df, transformations = transformations, add = FALSE)

  if (is_acc) {
    df = list(
      data = df,
      header = hdr,
      freq = freq,
      filename = filename,
      missingness = missingness
    )
    class(df) = "AccData"
  }
  df
}

#' @rdname flag_qc
#' @export
flag_qc_all = function(df, dynamic_range = NULL, verbose = TRUE,
                       flags = c("all", "spike", "interval_jump",
                                 "spike_second",
                                 "same_value", "device_limit",
                                 "all_zero", "impossible")) {
  transformations = get_transformations(df)
  transforms = NULL
  is_acc = is.AccData(df)
  if (is_acc) {
    hdr = df$header
    freq = df$freq
    filename = df$filename
    missingness = df$missingness
  }
  df = ensure_header_timestamp(df, subset = FALSE)
  if (any(startsWith(colnames(df), "flag"))) {
    warning(paste0(
      "Data has columns starting with flag - may affect results",
      " and column will be removed"))
  }
  flags = match.arg(flags, several.ok = TRUE)
  flag_names = c("spike", "interval_jump",
                 "spike_second", "same_value", "device_limit",
                 "all_zero", "impossible")
  all_flags = flag_names %in% flags
  names(all_flags) = flag_names
  if ("all" %in% flags) {
    all_flags[!all_flags] = TRUE
  }
  if (!any(all_flags)) {
    stop("No flags were identified, why are you running flag_qc_all?")
  }
  if (all_flags["spike"]) {
    if (verbose) {
      message("Flagging Spikes")
    }
    df = flag_spike(df)
    transforms = c("flag_spike", transforms)
  }
  if (all_flags["interval_jump"]) {
    if (verbose) {
      message("Flagging Interval Jumps")
    }
    df = flag_interval_jump(df, verbose = verbose > 1)
    transforms = c("flag_interval_jump", transforms)
  }
  if (all_flags["spike_second"]) {
    if (verbose) {
      message("Flagging Spikes at Second-level")
    }
    df = flag_spike_second(df)
    transforms = c("flag_spike_second", transforms)
  }
  if (all_flags["same_value"]) {
    if (verbose) {
      message("Flagging Repeated Values")
    }
    df = flag_same_value(df)
    transforms = c("flag_same_value", transforms)
  }
  if (all_flags["device_limit"]) {
    if (verbose) {
      message("Flagging Device Limit Values")
    }
    df = flag_device_limit(df, dynamic_range = dynamic_range)
    transforms = c("flag_device_limit", transforms)
  }
  if (all_flags["all_zero"]) {
    if (verbose) {
      message("Flagging Zero Values")
    }
    df = flag_all_zero(df)
    transforms = c("flag_all_zero", transforms)
  }
  if (all_flags["impossible"]) {
    if (verbose) {
      message("Flagging 'Impossible' Values")
    }
    df = flag_impossible(df)
    transforms = c("flag_impossible", transforms)
  }
  transforms = paste(transforms, collapse = ", ")
  transforms = paste0("flagging_data:", transforms)
  transformations = c(transforms, transformations)
  df = set_transformations(df, transformations = transformations, add = FALSE)

  if (is_acc) {
    df = list(
      data = df,
      header = hdr,
      freq = freq,
      filename = filename,
      missingness = missingness
    )
    class(df) = "AccData"
  }
  df
}
