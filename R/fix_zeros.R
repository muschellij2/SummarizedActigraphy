#' Fix Zeros from Idle Sleep Mode
#'
#' @param df An object with columns `X`, `Y`, and `Z` or an
#' object of class `AccData`
#' @param fill_in Should the zeros be filled in with the last
#' observation carried forward?
#' @param trim Should the time course be trimmed for zero values at
#' the beginning and the end of the time course?
#' @param by_second Should the last observation carried forward be done
#' only within the same second?
#'
#' @return A data set with the zeros filled in
#' @export
#' @examples
#' df = data.frame(
#'   X = c(0.3/sqrt(0.5), rep(0, 3)),
#'   Y = c(0.4/sqrt(0.5), rep(0, 3)),
#'   Z = c(0.5/sqrt(0.5), rep(0, 3)),
#'   stringsAsFactors = FALSE)
#' fix_zeros(df)
#' fix_zeros(df, fill_in = FALSE)
#' fix_zeros(df, trim = TRUE)
#' df$time = c(1,3,2, 4)
#' fix_zeros(df)
#' acc = list(header = NULL,
#' data = df
#' )
#' class(acc) = "AccData"
#' idle_na_locf(acc)
#' fix_zeros(acc, trim = TRUE)
fix_zeros = function(df,
                     fill_in = TRUE,
                     by_second = FALSE,
                     trim = FALSE) {
  transformations = get_transformations(df)
  transforms = NULL
  acc_data = is.AccData(df)
  if (acc_data) {
    xdf = df
    freq = df$freq
    df = df$data
  }
  firmware = attr(df, "firmware")
  sample_rate = attr(df, "sample_rate")
  acceleration_min = attr(df, "acceleration_min")
  acceleration_max = attr(df, "acceleration_max")

  df = sort_time_df(df)
  zero = rowSums(df[, c("X", "Y", "Z")] == 0) == 3

  if (trim) {
    not_zero = rle(!zero)
    not_zero$values[2:(length(not_zero$values) - 1)] = TRUE
    not_zero = inverse.rle(not_zero)
    zero = zero[not_zero]
    df = df[ not_zero, ]
    zero = rowSums(df[, c("X", "Y", "Z")] == 0) == 3
    transforms = c("trimmed", transforms)
  }

  names(zero) = NULL
  df$X[zero] = NA
  df$Y[zero] = NA
  df$Z[zero] = NA
  transforms = c("NA_zero_set", transforms)

  if (fill_in) {
    df =  idle_na_locf(df, by_second = by_second)
    transforms = c("filled_in", transforms)
  }

  transforms = paste(transforms, collapse = ", ")
  transforms = paste0("fix_zeros:", transforms)
  transformations = c(transforms, transformations)
  df = set_transformations(df, transformations = transformations,
                           add = FALSE)
  attr(df, "firmware") = firmware
  attr(df, "sample_rate") = sample_rate
  attr(df, "acceleration_min") = acceleration_min
  attr(df, "acceleration_max") = acceleration_max
  if (acc_data) {
    xdf$data = df
    xdf$freq = freq
    df = xdf
  }
  df
}

#' @rdname fix_zeros
#' @export
idle_na_locf = function(df, by_second = FALSE) {
  second = HEADER_TIME_STAMP = X = Y = Z = r = NULL
  rm(list= c("HEADER_TIME_STAMP", "X", "Y", "Z", "second"))
  acc_data = is.AccData(df)
  if (acc_data) {
    xdf = df
    df = df$data
  }
  df = sort_time_df(df)

  if (by_second) {
    df = ensure_header_timestamp(df)
    df = df %>%
      dplyr::mutate(second = floor_sec(HEADER_TIME_STAMP)) %>%
      dplyr::group_by(second)
  }

  df = df %>%
    tidyr::fill(X, Y, Z, .direction = "down")

  if (by_second) {
    df = df %>%
      dplyr::ungroup() %>%
      dplyr::select(-second) %>%
      dplyr::rename(time = HEADER_TIME_STAMP)
  }

  df$X[is.na(df$X)] = 0
  df$Y[is.na(df$Y)] = 0
  df$Z[is.na(df$Z)] = 0
  if (acc_data) {
    xdf$data = df
    df = xdf
  }
  df
}

sort_time_df = function(df) {
  if ("time" %in% names(df)) {
    if (is.unsorted(df$time)) {
      ord = order(df$time)
      if (!all(ord == 1:nrow(df))) {
        warning("Time is unsorted, will resort the data set")
        df = df[ ord, ]
      }
    }
  }
  df
}
