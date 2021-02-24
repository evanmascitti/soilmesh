#' @title Create file structure and empty data sheets for cleat mark testing
#'
#' @description Creates 2 directories and 2 additional data sheets. Designed to
#'   store penetrometer data, color photo indexes, and raw mesh files.
#'
#' @param experiment_ID character string uniquely identifying this set of soils or mixtures
#' @param soil_IDs character vector of the unique identifiers of the soils being
#'   tested (length 4)
#' @param date data collection date ("YYYY-MM-DD")
#'
#' @return writes new folders and empty data files to disk
#' @export
#'
new_cleat_mark_datasheets <- function(date, experiment_ID, soil_IDs){

 new_dirs <- c(paste0("analysis/data/raw_data/cleat_mark_testing/", date),
               (paste0("analysis/data/raw_data/cleat_mark_testing/",
                       date,
                       "/",
                       c("raw_meshes",
                         "color_photos"
                        )))
 )

 # check if directories already exist

 if(sum(mapply(dir.exists, new_dirs)) > 0){
   stop("\nOne or more of these directories already exist. Halting function call to prevent over-write.")
 }

 # create new directories to hold meshes and color images
 mapply(dir.create, new_dirs)


#  write empty drydown data sheet -----------------------------------------

 # construct file path for writing

 drydown_path <- paste0("analysis/data/raw_data/cleat_mark_testing/", date, "/", date, "_drydown_data.csv")

 # check if file is already present

 if(file.exists(drydown_path)){
   stop("\nDrydown file already exists. Halting function call to prevent over-write.")
 }

 # populate tibble
 drydown_tibble <- tibble::tibble(
   experiment_ID = experiment_ID,
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

 # write drydown file to disk
 readr::write_csv(x = drydown_tibble, file = drydown_path)
 message(crayon::green('\nPlease verify that the `drydown` datasheet was correctly written to disk.'))


# write empty color_photos index data sheet ------------------------------------------------------------


 # create empty file to keep track of the order of images, this way I don't need to
 # know their file names/numbers and I can test the cylinders out of order if needed
 # construct file path

 color_photos_path <- paste0("analysis/data/raw_data/cleat_mark_testing/", date, "/color_photos/", date, "_color_photos_index.csv")

 # check if file is already present

 if(file.exists(color_photos_path)){
   stop("\nColor photos index file already exists. Halting function call to prevent over-write.")
 }

 # build tibble

 color_photos_tibble <- tibble::tibble(experiment_ID = experiment_ID,
                                       date = date,
                                       test_order = 1:12,
                                       cylinder_ID = "")

 # write to disk

 readr::write_csv(x = color_photos_tibble, file = color_photos_path)
 message(crayon::green('\nPlease verify that the `color_photos_index` datasheet was correctly written to disk.'))


# write empty penetrometer data sheet ------------------------------------------------------------

 penetrometer_path <- paste0("analysis/data/raw_data/cleat_mark_testing/", date, "/", date, "_penetrometer_data.csv")

 # check if file is already present

 if(file.exists(penetrometer_path)){
    stop("\nColor photos index file already exists. Halting function call to prevent over-write.")
 }

 # build tibble

 penetrometer_tibble <- tidyr::crossing(experiment_ID = experiment_ID,
                                        date = date,
                                        cylinder_ID = c("01", "02", "03", "04", "05", "06", "07", "08","09", "10", "11", "12"),
                                        replicate = 1:4,
                                        penetrometer_reading = "")

 # write to disk

 readr::write_csv(x = penetrometer_tibble, file = penetrometer_path)
 message(crayon::green('\nPlease verify that the `penetrometer_data` datasheet was correctly written to disk.'))
}
