#' @export
#' @rdname calculate_measures
calculate_activity_counts = function(
  df,
  sample_rate = NULL,
  unit = "1 min",
  verbose = TRUE,
  ...) {

  if (!requireNamespace("activityCounts", quietly = TRUE)) {
    stop("activityCounts package required for calculating AC")
  }

  X = Y = Z = TIME = HEADER_TIME_STAMP = NULL
  rm(list = c("HEADER_TIME_STAMP", SummarizedActigraphy::xyz, "TIME"))


  df = ensure_header_timestamp(df, subset = TRUE)

  check = any(
    rowSums(df[SummarizedActigraphy::xyz] == 0, na.rm = TRUE) == 3,
    na.rm = TRUE)
  if (check) {
    warning("Rows of zeros detected, should likely run fix_zeros on the data")
  }
  sample_rate = get_sample_rate(df, sample_rate)

  if (verbose) {
    message("Calculating Axis-Specific Activity Counts")
  }
  df = activityCounts::counts(
    data = as.data.frame(df),
    hertz = sample_rate,
    x_axis = which(colnames(df) == "X"),
    y_axis = which(colnames(df) == "Y"),
    z_axis = which(colnames(df) == "Z"),
    time_column = which(colnames(df) == "HEADER_TIME_STAMP"),
    start_time =  df[["HEADER_TIME_STAMP"]][[1]]
  )
  colnames(df) = toupper(colnames(df))
  df = df %>% dplyr::rename(
    HEADER_TIME_STAMP = TIME)

  if (verbose) {
    message("Summing up Axes")
  }
  df = df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      HEADER_TIME_STAMP = lubridate::floor_date(HEADER_TIME_STAMP,
                                                unit)) %>%
    dplyr::group_by(HEADER_TIME_STAMP) %>%
    dplyr::summarise(
      dplyr::across(c(X, Y, Z), sum)
    ) %>%
    dplyr::mutate(
      AC = sqrt(X^2 + Y^2 + Z^2)
    )

  df

}

#' @export
#' @rdname calculate_measures
calculate_ac = calculate_activity_counts
