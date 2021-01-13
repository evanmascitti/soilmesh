#' @title Populate a skeleton csv file for data collection with cleat-mark
#'   device
#'
#' @description Ensures consistent column names and rectangular format across
#'   days and soils. Records information needed to calculate water content at two sampling times.
#'
#' @details This format allows a water content sample to be extracted at the
#'   beginning of the dry-down process and a second sample to be extracted
#'   immediately after testing. This allows the total water loss to be computed
#'   over the course of the test. Although the rate of water loss may vary as a
#'   function of water content, this is still a useful data point for estimating
#'   evaporation rate, and also because it allows a check to be performed on the
#'   uniformity of water content across cylinders of the same soil at the
#'   beginning of the test.
#'
#'
#' @param soil_IDs character vector of the unique identifiers of the soils being
#'   tested (length 4)
#' @param date quoted date of data collection, in yyyy-mm-dd format
#' @param dir directory in which to save the .csv file (no trailing slash)
#'
#' @return File is written to disk and a message is printed in the console.
#' @export
#'
cleatmark_water_contents_file <- function(soil_IDs, date, dir){

  data_tibble <- tibble::tibble(
    soil_ID = rep(rep(soil_IDs, each = 3), times=2),
    cylinder_ID = rep(1:12, times=2),
    date = date,
    time_type = rep(c("lamp_on", "test_time"), each=12),
    time = "",
    tin_tare_set = "",
    tin_number = "",
    tin_w_wet_sample= "",
    tin_w_OD_sample = "",
    sand_cup_mass_before_filling_g = "-",
    sand_cup_mass_after_filling_g = "-",
    OD_soil_mass_above_ref_plane = "-",
    comments = "-"
    )


  data_tibble %>%
    readr::write_csv(file = paste0(dir, "/", date, "_cleatmark_w_conts_and_metadata.csv") )

  message(crayon::green('Please verify that the file was correctly written to disk.'))
}
