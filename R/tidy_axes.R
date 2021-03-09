#' Tidy axes to a long format
#'
#' @param df An object with columns a time column `X`, `Y`, and `Z` or an
#' object of class `AccData`
#' @param only_xyz should the data be subset for only X/Y/Z values (and time)?
#' @return A long data set with `time`, `axis`, and `value`
#' @export
#'
#' @examples
#' file = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
#' package = "SummarizedActigraphy")
#' res = read_actigraphy(file)
#' long = tidy_axes(res)
tidy_axes = function(df, only_xyz = TRUE) {
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr required for tidy_axes")
  }
  time = HEADER_TIME_STAMP = X = Y = Z = NULL
  rm(list = c("HEADER_TIME_STAMP", "X", "Y", "Z", "time"))
  df = ensure_header_timestamp(df, subset = FALSE)
  cn = colnames(df)
  if (!"time" %in% cn && "HEADER_TIME_STAMP" %in% cn) {
    df = df %>%
      dplyr::rename(time = HEADER_TIME_STAMP)
  }
  if (only_xyz) {
    df = df %>%
      dplyr::select(time, X, Y, Z)
  }
  df = df %>%
    tidyr::gather("axis", "value", -time)
}
