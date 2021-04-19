tsibbler = function(x, transformations = NULL) {
  time = enmo = mad = X = Y = Z = NULL
  rm(list = c("X", "Y", "Z", "enmo", "mad", "time"))
  if (is.null(transformations)) {
    transformations = get_transformations(x)
  }

  x = tsibble::build_tsibble(x, index = time)
  x = set_transformations(x, transformations, add = FALSE)
  x
}
#' Summarize Actigraphy Data
#'
#' @param x an AccData object.  If `x` is a character, then
#' \code{\link{read_actigraphy}} will be run
#' @param ... Additional arguments to pass to
#' \code{\link{read_actigraphy}}
#' @param verbose print diagnostic messages
#' @param unit units to group the data to take the statistic over
#' @param fix_zeros Should \code{\link{fix_zeros}} be run before calculating
#' the measures?
#' @param fill_in if \code{fix_zeros = TRUE}, should the zeros be
#' filled in with the last
#' observation carried forward?
#' @param trim if \code{fix_zeros = TRUE},
#' should the time course be trimmed for zero values at
#' the beginning and the end of the time course?
#' observation carried forward?
#' @param calculate_mims Should MIMS units be calculated?
#' Passed to \code{\link{calculate_measures}}
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
#'
#' @return A \code{tsibble} object, with 86400 rows,
#' with one row for each second of the day `24*60*60`.
#' @importFrom stats mad sd median var
#' @importFrom dplyr summarize vars
#' @importFrom dplyr ungroup summarise group_by mutate summarize_at
#' @importFrom tibble as_tibble
#' @importFrom tsibble build_tsibble
#' @importFrom hms hms
#' @importFrom lubridate floor_date
#'
#' @examples
#' path = system.file("extdata",
#' "TAS1H30182785_2019-09-17.gt3x",
#' package = "SummarizedActigraphy")
#'
#' x = read_actigraphy(path)
#'
#' options(digit.secs = 2)
#'
#' fixed = fix_zeros(x)
#' daily = summarize_daily_actigraphy(fixed, fix_zeros = FALSE)
#' average_day = collapse_daily_actigraphy(daily)
#' \dontrun{
#'   average_day = summarize_actigraphy(fixed, fix_zeros = FALSE)
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     library(magrittr)
#'     average_day %>%
#'       ggplot(aes(x = time, y = ai_mean)) +
#'       geom_line()
#'
#'     average_day %>%
#'       ggplot(aes(x = time, y = ai_median)) +
#'       geom_line()
#'   }
#' }
#' @rdname summarize_actigraphy
#' @export
summarize_daily_actigraphy = function(
  x,
  unit = "1 min",
  fix_zeros = TRUE,
  fill_in = TRUE,
  trim = FALSE,
  verbose = TRUE,
  calculate_mims = FALSE,
  calculate_ac = FALSE,
  flag_data = TRUE,
  ensure_all_time = TRUE,
  flags = NULL,
  ...) {
  time = enmo = mad = X = Y = Z = NULL
  rm(list = c("X", "Y", "Z", "enmo", "mad", "time"))

  if (is.character(x)) {
    x = read_actigraphy(x, verbose = verbose, ...)
  }

  x = calculate_measures(
    x,
    unit = unit,
    fix_zeros = fix_zeros,
    fill_in = fill_in,
    trim = trim,
    calculate_mims = calculate_mims,
    calculate_ac = calculate_ac,
    flag_data = flag_data,
    flags = flags,
    ensure_all_time = ensure_all_time,
    verbose = verbose)
  x$X = x$Y = x$Z = NULL
  x$AUC_X = x$AUC_Y = x$AUC_Z = NULL

  x = tsibbler(x)
  return(x)
}

#' @export
#' @rdname summarize_actigraphy
summarise_daily_actigraphy = summarize_daily_actigraphy


#' @rdname summarize_actigraphy
#' @param .fns Functions to apply to each of the selected columns.
#' See \code{\link{across}}
#' @export
summarize_actigraphy = function(
  x,
  unit = "1 min",
  .fns = list(mean = mean, median = median),
  verbose = TRUE,
  ...) {

  first_day = NULL
  mean_r = time = ai = enmo = mad = NULL
  rm(list = c("first_day", "ai", "time", "enmo", "mad"))

  if (verbose) {
    message("Running Daily Actigraphy")
  }
  x = summarize_daily_actigraphy(x, unit = unit,
                                 verbose = verbose > 1,
                                 ...)
  x = collapse_daily_actigraphy(x, .fns = .fns, verbose = verbose)

  x
}

#' @rdname summarize_actigraphy
#' @export
collapse_daily_actigraphy = function(
  x,
  .fns = list(mean = mean, median = median),
  verbose = TRUE) {

  transformations = get_transformations(x)

  first_day = NULL
  mean_r = time = ai = enmo = mad = NULL
  rm(list = c("first_day", "ai", "time", "enmo", "mad"))

  x = tibble::as_tibble(x) %>%
    dplyr::ungroup()

  if (verbose) {
    message("Getting the First Day")
  }
  x = x %>%
    dplyr::mutate(first_day = lubridate::floor_date(time, unit = "day"))

  x = x %>%
    dplyr::mutate(time = hms::hms(as.numeric(time - first_day,
                                             unit = "secs")))

  if (verbose) {
    message("Summarizing Data")
  }
  # average day data
  x = x %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::any_of(c("AI", "SD", "MAD", "MEDAD",
                        "ENMO_t", "AUC", "AC",
                        "mean_r", "MIMS_UNIT")),
        .fns = .fns,
        na.rm = TRUE)
    )

  x = tsibbler(x, transformations = transformations)
  x
}

#' @export
#' @rdname summarize_actigraphy
summarise_actigraphy = summarize_actigraphy



