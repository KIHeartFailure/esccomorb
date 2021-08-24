# default is to use tidyverse functions
select <- dplyr::select 
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

# colours 
global_kicols <- c(
  grDevices::rgb(135, 0, 82, maxColorValue = 255),
  grDevices::rgb(212, 9, 99, maxColorValue = 255),
  grDevices::rgb(128, 128, 128, maxColorValue = 255),
  grDevices::rgb(151, 216, 218, maxColorValue = 255), # aqua
  grDevices::rgb(136, 196, 197, maxColorValue = 255), # teal
  grDevices::rgb(189, 171, 179, maxColorValue = 255) # lavender
)

# used for calculation of ci 
global_z05 <- qnorm(1 - 0.025)