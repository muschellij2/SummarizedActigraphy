

#' Standardize the Accelerometry Data
#'
#' @param data A `data.frame` with a column for time in `POSIXct` (usually
#' `time`), and `X`, `Y`, `Z`
#' @param subset should only the `HEADER_TIMESTAMP` (if available)
#' and `XYZ` be subset?
#'
#' @return A `data.frame` with `X/Y/Z` and a time in
#' `time` (if available).
#' @export
standardize_data = function(data, subset = TRUE) {
  HEADER_TIMESTAMP = TIME = HEADER_TIME_STAMP = X = Y = Z = NULL
  rm(list = c("HEADER_TIMESTAMP", "HEADER_TIME_STAMP", "X", "Y", "Z",
              "TIME"))
  if (is.matrix(data)) {
    if (is.numeric(data)) {
      stopifnot(ncol(data) == 3)
      data = as.data.frame(data)
      colnames(data) = c("X", "Y", "Z")
    } else {
      stop("data is a matrix and cannot be coerced to necessary structure")
    }
  }
  # uppercase
  colnames(data) = toupper(colnames(data))
  cn = colnames(data)
  if ("HEADER_TIMESTAMP" %in% cn && !"TIME" %in% cn) {
    data = data %>%
      dplyr::rename(TIME = HEADER_TIMESTAMP)
  }
  if ("HEADER_TIME_STAMP" %in% cn && !"TIME" %in% cn) {
    data = data %>%
      dplyr::rename(TIME = HEADER_TIME_STAMP)
  }
  if ("TIME" %in% colnames(data)) {
    if (is.unsorted(data$TIME)) {
      stop("Time in data must be sorted before running!")
    }
  }
  if (subset) {
    data = data %>%
      dplyr::select(dplyr::any_of("HEADER_TIMESTAMP"), X, Y, Z)
  }
  stopifnot(all(c("X", "Y", "Z") %in% colnames(data)))
  data
}

#' @export
#' @rdname standardize_data
standardise_data = standardize_data

xyz_data = function(data) {
  data = standardize_data(data)
  as.matrix(data[, c("X", "Y", "Z"), drop = FALSE])
}
