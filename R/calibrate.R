#' Calibrate Accelerometer Data using GGIR
#'
#' @param file Either a GT3X file, `AccData` object, or `data.frame` with
#' `X/Y/Z` and `time`
#' @param verbose print diagnostic messages, higher number result in higher verbosity
#' @param ... Additional arguments to pass to \code{\link{g.calibrate}}
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
#'
#'
#' @return A set of calibration coefficients
#' @export
#' @rdname calibrate
#' @examples
#' \donttest{
#' files = read.gt3x::gt3x_datapath(2)
#' path = files[1]
#' res = read_actigraphy(path)
#' cab = calibrate(res)
#' }
estimate_calibration_values = function(file, verbose = TRUE,
                                       fix_zeros = TRUE,
                                       fill_in = TRUE,
                                       by_second = FALSE,
                                       trim = FALSE,
                                       ...) {

  if (is.character(file) && grepl("[.]gt3x(|[.]gz)$", file)) {
    if (verbose) {
      message("Detected gt3x file - reading in using read_actigraphy")
    }
    file = read_actigraphy(file, verbose = verbose > 1)
  }

  if (fix_zeros) {
    if (verbose) {
      message("Fixing Zeros")
    }
    file = fix_zeros(
      file,
      fill_in = fill_in,
      by_second = by_second,
      trim = trim)
  }
  if (!is.character(file)) {
    if (!is.AccData(file) && !is.data.frame(file)) {
      warning("file is not an AccData or data.frame - may not work!")
    }
    if (verbose) {
      message("Writing out file for GGIR")
    }
    file = write_acc_csv(file)
  }
  if (verbose) {
    message("Running g.inspectfile")
  }
  inspectfileobject = GGIR::g.inspectfile(file)
  filequality = data.frame(
    filetooshort = FALSE,
    filecorrupt = FALSE,
    filedoesnotholdday = FALSE)
  if (verbose) {
    message("Running g.calibrate")
  }
  C = GGIR::g.calibrate(file, ...)
  return(C)
}

#' @export
#' @param round_after_calibration Should the data be rounded after calibration?
#' Will round to 3 digits
#' @rdname calibrate
calibrate = function(file, verbose = TRUE,
                     fix_zeros = TRUE,
                     fill_in = TRUE,
                     by_second = FALSE,
                     trim = FALSE,
                     round_after_calibration = TRUE,
                     ...) {
  if (is.character(file) && grepl("[.]gt3x(|[.]gz)$", file)) {
    if (verbose) {
      message("Detected gt3x file - reading in using read_actigraphy")
    }
    file = read_actigraphy(file, verbose = verbose > 1)
  }
  # running it here because then we can add the transforms in there
  if (fix_zeros) {
    if (verbose) {
      message("Fixing Zeros")
    }
    file = fix_zeros(
      file,
      fill_in = fill_in,
      by_second = by_second,
      trim = trim)
  }
  # already done - don't want to add another transformations
  fix_zeros = FALSE
  transformations = get_transformations(file)

  vals = estimate_calibration_values(
    file, verbose = verbose,
    fix_zeros = fix_zeros,
    fill_in = fill_in,
    by_second = by_second,
    trim = trim,
    ...)

  acc_data = is.AccData(file)
  if (acc_data) {
    xfile = file
    file = file$data
    xfile$data = NA
  }
  scale_value = vals$scale
  offset_value = vals$offset
  file[, SummarizedActigraphy::xyz] = scale(file[, SummarizedActigraphy::xyz],
                                            center = -offset_value,
                                            scale = 1/scale_value)

  if (round_after_calibration) {
    for (i in SummarizedActigraphy::xyz) {
      file[[i]] = round(file[[i]], 3)
    }
  }
  attr(file, "calibration_values") = vals

  if (acc_data) {
    xfile$data = file
    rm(file)
    file = xfile
    rm(xfile)
  }

  file = set_transformations(file,
                             transformations = "GGIR_calibrated",
                             add = TRUE)

  return(file)
}
