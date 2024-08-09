
as_convert_safe = function(x, ..., func = lubridate::as_datetime) {
  nax = is.na(x)
  xx = func(x, ...)
  naxx = is.na(xx)
  any_na = !nax & naxx
  if (any(any_na)) {
    message("Conversion not done for:")
    print(x[any_na])
    stop("conversion failed")
  }
  xx
}

as_date_safe = function(x, ...) {
  as_convert_safe(x, ..., func = lubridate::as_date)
}

as_datetime_safe = function(x, ...) {
  as_convert_safe(x, ..., func = lubridate::as_datetime)
}

#' Create Day-Level Inclusion information
#'
#' @param df A `data.frame` with the columns `time`
#' @param min_required Number of minutes required in a day to be called `included`
#'
#' @return A `data.frame` for each day with information of number of minutes observed
#' and included
#' @export
#'
#' @examples
#' file = system.file("extdata", "TAS1H30182785_2019-09-17.gt3x",
#'                    package = "SummarizedActigraphy")
#' df = read_actigraphy(file)
#' df = df$data
#' df = df %>%
#'   dplyr::mutate(r = sqrt(X^2 + Y^2 + Z^2),
#'                 time = lubridate::floor_date(time, "1 minute")) %>%
#'   dplyr::group_by(time) %>%
#'   dplyr::summarise(r = sum(r), .groups = "drop") %>%
#'   dplyr::mutate(wear = r > 4000)
#'
#' res = create_day_inclusion(df)
create_day_inclusion = function(
    df,
    min_required = 1368L
) {
  n_minutes_observed = minute = n_minutes_wear = wear = hourtime = NULL
  rm(list = c("n_minutes_wear", "wear", "hourtime", "minute",
              "n_minutes_observed"))
  HEADER_TIME_STAMP = observed = NULL
  rm(list = c("observed", "HEADER_TIME_STAMP"))
  df = ensure_header_timestamp(df, subset = FALSE)

  sd = setdiff(c("HEADER_TIME_STAMP", "wear"), colnames(df))
  if (length(sd) > 0) {
    cn = paste(sd, collapse = ", ")
    stop(paste0("Columns ", cn, " are not present in the data"))
  }
  df = df %>%
    dplyr::rename(time = HEADER_TIME_STAMP)
  df = df %>%
    dplyr::mutate(
      date = as_date_safe(time),
      minute = lubridate::floor_date(time, unit = "minutes"),
      hourtime = hms::as_hms(time))

  # make distinct for summarization
  df = df %>%
    dplyr::distinct(date, minute, hourtime, wear)

  # Check for duplicates - should not be there
  suppressMessages({
    dupes = janitor::get_dupes(df, date, minute)
  })
  if (nrow(dupes) > 0) {
    stop("There are duplicate rows of date and minute!")
  }

  suppressMessages({
    dupes = janitor::get_dupes(df, date, hourtime)
  })
  if (nrow(dupes) > 0) {
    stop("There are duplicate rows of date and minute!")
  }
  all_minutes = expand.grid(
    date = unique(df$date),
    hourtime = hms::hms(minutes = 1:1440),
    observed = TRUE
  )
  df = df %>%
    dplyr::full_join(all_minutes) %>%
    tidyr::replace_na(list(wear = FALSE,
                           observed = FALSE))

  # summarise
  res = df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
      n_minutes_wear = sum(wear),
      n_minutes_observed = sum(observed),
    ) %>%
    dplyr::mutate(
      prop_minutes_wear = n_minutes_wear / 1440L,
      prop_minutes_wear_from_observed = n_minutes_wear / n_minutes_observed
    ) %>%
    dplyr::mutate(
      is_included = n_minutes_wear >= min_required
    )

  return(res)
}
