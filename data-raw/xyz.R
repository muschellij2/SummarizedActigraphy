## code to prepare `DATASET` dataset goes here
xyz = c("X", "Y", "Z")

usethis::use_data(xyz, overwrite = TRUE)

txyz = c("time", "X", "Y", "Z")

usethis::use_data(txyz, overwrite = TRUE)


xyzt = c("X", "Y", "Z", "time")

usethis::use_data(xyzt, overwrite = TRUE)
