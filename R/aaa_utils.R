is.AccData = function(x) {
  inherits(x, "AccData")
}


ticks2datetime = function (ticks, tz = "GMT")
{
  ticks <- as.numeric(ticks)
  seconds <- ticks/1e+07
  datetime <- as.POSIXct(seconds, origin = "0001-01-01", tz = tz)
  datetime
}


quick_check = function(df) {
  if (is.AccData(df)) {
    df = df$data
  }
  HEADER_TIME_STAMP = Axis1 = Axis2 = Axis3 = NULL
  rm(list= c("HEADER_TIME_STAMP", "Axis1", "Axis2", "Axis3"))
  df = df %>%
    dplyr::mutate(VM_check = round(
      sqrt(Axis1^2 + Axis2^2 + Axis3^2), 2))
  stopifnot(max(abs(df$VM_check - df$`Vector Magnitude`)) <
              1e-5)
}
