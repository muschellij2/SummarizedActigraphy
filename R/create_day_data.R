#' Summarize Actigraphy Data
#'
#' @param x an AccData object.  If `x` is a character, then
#' \code{\link{read_actigraphy}} will be run
#' @param ... Additional arguments to pass to
#' \code{\link{read_actigraphy}}
#' @param verbose print diagnostic messages
#' @param units units to group the data to take the statistic over
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
  units = "1 seconds",
  verbose = TRUE,
  ...) {
  time = enmo = mad = X = Y = Z = NULL
  rm(list = c("X", "Y", "Z", "enmo", "mad", "time"))

  if (is.character(x)) {
    x = read_actigraphy(x, ...)
  }
  if ("data.out" %in% names(x)) {
    x = x$data.out
  }
  if (verbose) {
    message("Calculating ENMO")
  }
  if (verbose) {
    message("Rounding Time Column")
  }
  x$time = lubridate::floor_date(x$time, unit = units)

  if (verbose) {
    message("Calculating Statistics")
  }
  x = x %>%
    mutate(enmo = sqrt(X^2 + Y^2 + Z^2) - 1)
  if (any(is.na(x$enmo))) {
    warning("There were NA values in ENMO")
  }
  x = x %>%
    group_by(time) %>%
    summarize(
      mad = (mad(X, na.rm = TRUE) + mad(Y, na.rm = TRUE) + mad(Z, na.rm = TRUE))/3,
      ai = sqrt((var(X, na.rm = TRUE) + var(Y, na.rm = TRUE) + var(Z, na.rm = TRUE))/3),
      n_values = sum(!is.na(enmo)),
      sd_enmo = sd(enmo, na.rm = TRUE),
      enmo = mean(enmo, na.rm = TRUE)
    )

  ts = tsibble::build_tsibble(x,
                              index = time)
  return(ts)
}

#' @export
#' @rdname summarize_actigraphy
summarise_daily_actigraphy = summarize_daily_actigraphy


#' @rdname summarize_actigraphy
#' @export
summarize_actigraphy = function(
  x,
  units = "1 seconds",
  verbose = TRUE,
  ...) {

  first_day = NULL
  time = ai = enmo = mad = NULL
  rm(list = c("first_day", "ai", "time", "enmo", "mad"))


  x = summarize_daily_actigraphy(x, units = units, verbose = verbose,...)
  x = tibble::as_tibble(x) %>%
    dplyr::ungroup()

  if (verbose) {
    message("Getting the First Day")
  }
  x = x %>%
    mutate(first_day = lubridate::floor_date(time, unit = "day"))

  x = x %>%
    mutate(time = hms::hms(as.numeric(time - first_day,
                                           unit = "secs")))

  # average_day = x %>%
  #   group_by(time_only) %>%
  #   summarize_at(.vars = vars(ai, mad, enmo),
  #                mean, na.rm = TRUE)

  if (verbose) {
    message("Summarizing Data")
  }
  average_day = x %>%
    group_by(time) %>%
    summarize_at(.vars = vars(ai, mad, enmo),
                 list(mean = mean, median = median), na.rm = TRUE)

  ts = tsibble::build_tsibble(average_day,
                              index = time)
  ts
}

#' @export
#' @rdname summarize_actigraphy
summarise_actigraphy = summarize_actigraphy



