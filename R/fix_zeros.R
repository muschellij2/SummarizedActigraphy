#' Fix Zeros from Idle Sleep Mode
#'
#' @param df An object with columns `X`, `Y`, and `Z` or an
#' object of class `AccData`
#' @param fill_in Should the zeros be filled in with the last
#' observation carried forward?
#' @param trim Should the time course be trimmed for zero values at
#' the beginning and the end of the time course?
#'
#' @return A data set with the zeros filled in
#' @export
#' @importFrom zoo na.locf
#' @examples
#' df = data.frame(
#'   X = c(0.3/sqrt(0.5), rep(0, 3)),
#'   Y = c(0.4/sqrt(0.5), rep(0, 3)),
#'   Z = c(0.5/sqrt(0.5), rep(0, 3)),
#'   stringsAsFactors = FALSE)
#' fix_zeros(df)
#' fix_zeros(df, fill_in = FALSE)
#' fix_zeros(df, trim = TRUE)
#' df$time = rnorm(nrow(df))
#' fix_zeros(df)
#' acc = list(header = NULL,
#' data = df
#' )
#' class(acc) = "AccData"
#' fix_zeros(acc, trim = TRUE)
fix_zeros = function(df, fill_in = TRUE,
                     trim = FALSE) {
  acc_data = is.AccData(df)
  if (acc_data) {
    xdf = df
    df = df$data
  }
  if ("time" %in% names(df)) {
    if (!is.unsorted(df$time)) {
      ord = order(df$time)
      if (!all(ord == 1:nrow(df))) {
        warning("Time is unsorted, will resort the data set")
        gt3x = gt3x[ ord, ]
      }
    }
  }
  zero = rowSums(df[, c("X", "Y", "Z")] == 0) == 3
  if (trim) {
    not_zero = rle(!zero)
    not_zero$values[2:(length(not_zero$values)-1)] = TRUE
    not_zero = inverse.rle(not_zero)
    zero = zero[not_zero]
    df = df[ not_zero, ]
  }

  names(zero) = NULL
  df$X[zero] = NA
  df$Y[zero] = NA
  df$Z[zero] = NA
  if (fill_in) {
    df =  idle_na_locf(df)
  }
  if (acc_data) {
    xdf$data = df
    df = xdf
  }
  df
}

#' @rdname fix_zeros
#' @export
idle_na_locf = function(df) {
  acc_data = is.AccData(df)
  if (acc_data) {
    xdf = df
    df = df$data
  }
  df$X = zoo::na.locf(df$X, na.rm = FALSE)
  df$Y = zoo::na.locf(df$Y, na.rm = FALSE)
  df$Z = zoo::na.locf(df$Z, na.rm = FALSE)

  df$X[is.na(df$X)] = 0
  df$Y[is.na(df$Y)] = 0
  df$Z[is.na(df$Z)] = 0
  if (acc_data) {
    xdf$data = df
    df = xdf
  }
  df
}
