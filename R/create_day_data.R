#' Summarize Actigraphy Data
#'
#' @param x an AccData object.  If `x` is a character, then
#' \code{\link{read_actigraphy}} will be run
#' @param ... Additional arguments to pass to
#' \code{\link{read_actigraphy}}
#' @param verbose print diagnostic messages
#' @param units units to group the data to take the statistic over
#' @param fix_zeros Should \code{\link{fix_zeros}} be run before calculating
#' the measures?
#' @param fill_in if \code{fix_zeros = TRUE}, should the zeros be
#' filled in with the last
#' observation carried forward?
#'
#' @return A \code{tsibble} object, with 86400 rows,
#' with one row for each secon d of the day `24*60*60`.
#' @importFrom stats mad sd median var
#' @importFrom dplyr summarize vars
#' @importFrom dplyr ungroup summarise group_by mutate summarize_at
#' @importFrom tibble as_tibble
#' @importFrom tsibble build_tsibble
#' @importFrom hms hms
#' @importFrom lubridate floor_date
#'
#' @examples
#' url = "https://github.com/THLfi/read.gt3x/files/3522749/GT3X%2B.01.day.gt3x.zip"
#' destfile = tempfile(fileext = ".zip")
#' dl = utils::download.file(url, destfile = destfile)
#' gt3x_file = utils::unzip(destfile, exdir = tempdir())
#' gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
#' path = gt3x_file
#'
#' x = read_actigraphy(path)
#'
#' library(dplyr)
#' library(lubridate)
#' options(digit.secs = 2)
#'
#' daily = summarize_daily_actigraphy(x)
#' average_day = summarize_actigraphy(x)
#' \dontrun{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#' library(magrittr)
#' average_day %>%
#'   ggplot(aes(x = time, y = ai_mean)) +
#'   geom_line()
#'
#' average_day %>%
#'   ggplot(aes(x = time, y = ai_median)) +
#'   geom_line()
#' }
#' }
#' @rdname summarize_actigraphy
#' @export
summarize_daily_actigraphy = function(
  x,
  units = "1 min",
  fix_zeros = TRUE,
  fill_in = TRUE,
  verbose = TRUE,
  ...) {
  time = enmo = mad = X = Y = Z = NULL
  rm(list = c("X", "Y", "Z", "enmo", "mad", "time"))

  if (is.character(x)) {
    x = read_actigraphy(x, ...)
  }

  x = calculate_measures(
    x,
    epoch = units,
    fix_zeros = fix_zeros,
    fill_in = fill_in,
    calculate_mims = FALSE,
    verbose = verbose)

  ts = tsibble::build_tsibble(x,
                              index = time)
  return(ts)
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
  units = "1 min",
  .fns = list(mean = mean, median = median),
  verbose = TRUE,
  ...) {

  first_day = NULL
  mean_r = time = ai = enmo = mad = NULL
  rm(list = c("first_day", "ai", "time", "enmo", "mad"))

  if (verbose) {
    message("Running Daily Actigraphy")
  }
  x = summarize_daily_actigraphy(x, units = units,
                                 verbose = verbose > 1,
                                 ...)
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
  average_day = x %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::one_of("AI", "SD", "MAD", "MEDAD",
                      "mean_r", "MIMS_UNIT"),
        .fns = .fns,
        na.rm = TRUE)
    )

  ts = tsibble::build_tsibble(average_day,
                              index = time)
  ts
}

#' @export
#' @rdname summarize_actigraphy
summarise_actigraphy = summarize_actigraphy



