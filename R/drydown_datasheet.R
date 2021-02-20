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
#'   beginning of the test. See [`drydown_analysis()`].
#'
#'
#' @param soil_IDs character vector of the unique identifiers of the soils being
#'   tested (length 4)
#' @param date date of data collection ("yyyy-mm-dd")
#' @param dir directory in which to save the .csv file
#'
#' @return File is written to disk and a message is printed in the console.
#' @export
#'
drydown_datasheet <- function(soil_IDs, date, dir){

  data_tibble <- tibble::tibble(
    soil_ID = rep(rep(soil_IDs, each = 3), times=2),
    cylinder_ID = rep(1:12, times=2),
    date = date,
    time_type = rep(c("lamp_on", "test_time"), each=12),
    time = "",
    AM_PM = "",
    tin_tare_set = "",
    tin_number = "",
    tin_w_wet_sample= "",
    tin_w_OD_sample = "",
    comments = "-"
    )

  if(stringr::str_sub(dir, start = -1) == "/"){
    outfile <- paste0(dir, date, "_drydown_data.csv") } else{
      outfile <- paste0(dir, "/", date, "_drydown_data.csv")
    }

  data_tibble %>%
    readr::write_csv(file = outfile)

  message(crayon::green('Please verify that the file was correctly written to disk.'))
}
