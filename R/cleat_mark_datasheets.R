#' @title Create file structure and empty data sheets for cleat mark testing
#'
#' @description Creates 2 directories and 2 additional data sheets. Designed to
#'   store penetrometer data, color photo indexes, and raw mesh files. A new
#'   directory is created for the date of testing, and this holds all files
#'   related to cleat-mark testing for that day. are created relative to root or
#'   current working directory. The by-date directories should live under
#'   `ecmdata/raw-data/cleat-mark-testing/` for a given project.
#'
#' @param experiment_name character string uniquely identifying this set of
#'   soils or mixtures
#' @param sample_names character vector of the unique identifiers of the soils
#'   being tested (length 4)
#' @param date data collection date ("YYYY-MM-DD")
#' @param tin_tare_set unique ID for the set of tins used to measure water
#'   content; see [asi468::tin_tares]
#' @param bowl_tare_set unique ID for the set of stainless steel bowls used to measure water
#'   content; see [asi468::stainless_bowl_tares]
#' @param sand_loose_density Value for loose dry density of material used to
#'   fill holes. Default value stored as [asi468::vrc_sand_loose_density], which
#'   is 1.54.
#' @param mini_density_reps number of replicate specimens extracted per cylinder
#'   for the miniature sand-cone method
#'
#' @return Writes new folders and empty data files to disk.
#' @export
#'
cleat_mark_datasheets <- function(experiment_name, sample_names, date, tin_tare_set,
                                  bowl_tare_set, sand_loose_density = asi468::vrc_sand_loose_density,
                                  mini_density_reps = 1){


# build paths to three new directories: one for everything that will be
# collected today, and sub-directories under this one for the raw meshes and the
# photos. The mesh files will be manually copied into this directory at the end
# of the day, either with point-and-click or potentially using a helper function
# which I haven't yet written.

 new_dirs <- c(
     here::here("ecmdata/raw-data/cleat-mark-testing", date),
     here::here("ecmdata/raw-data/cleat-mark-testing",
                               date, "raw-meshes"),
     here::here("ecmdata/raw-data/cleat-mark-testing",
                date, "color-photos"),
     here::here("ecmdata/raw-data/cleat-mark-testing",
                date, "other-data")
     )

  # check if directories already exist

 if(sum(purrr::map_lgl(new_dirs, dir.exists)) > 0){
   stop("\nOne or more of these directories already exist. Halting function call to prevent over-write.")
 }

 # create new directories to hold meshes and color images
 lapply(new_dirs, dir.create)


#  write empty drydown data sheet -----------------------------------------

 # construct file path for writing

 drydown_path <- here::here("ecmdata/raw-data/cleat-mark-testing/other-data", date, glue::glue("drydown-data_{date}.csv"))

 # populate tibble
 drydown_tibble <- tibble::tibble(
    experiment_name = experiment_name,
    date = date,
    sample_name = rep(rep(sample_names, each = 3), times=2),
    cylinder_ID = rep(1:12, times=2),
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

 color_photos_path <- here::here("ecmdata/raw-data/cleat-mark-testing",
                                 date,
                                 "color-photos",
                                 glue::glue("color-photos-index_{date}.csv"))

 # build tibble

 color_photos_tibble <- tibble::tibble(
    experiment_name = experiment_name,
    date = date,
    test_order = 1:12,
    cylinder_ID = "")

 # write to disk

 readr::write_csv(x = color_photos_tibble, file = color_photos_path)
 message(crayon::green('\nPlease verify that the `color_photos_index` datasheet was correctly written to disk.'))


# write empty penetrometer data sheet ------------------------------------------------------------

 penetrometer_path <- here::here("ecmdata/raw-data/cleat-mark-testing/other-data",
                                 date,
                                 glue::glue("penetrometer-data_{date}.csv"))

 # build tibble

 penetrometer_tibble <- tidyr::crossing(experiment_name = experiment_name,
                                        date = date,
                                        cylinder_ID = c("01", "02", "03", "04", "05", "06", "07", "08","09", "10", "11", "12"),
                                        replication = 1:4,
                                        penetrometer_reading = "") %>%
     dplyr::arrange(.data$cylinder_ID)

 # write to disk

 readr::write_csv(x = penetrometer_tibble, file = penetrometer_path)
 message(crayon::green('\nPlease verify that the `penetrometer_data` datasheet was correctly written to disk.'))


# mini-density data -------------------------------------------------------

 mini_density_tibble <- tidyr::crossing(
     experiment_name = experiment_name,
     date = date,
     cylinder_ID = c("01", "02", "03", "04", "05", "06", "07", "08","09", "10", "11", "12"),
     replication = 1:mini_density_reps,
     sand_loose_density = sand_loose_density,
     tin_tare_set = tin_tare_set,
     tin_number = "",
     sand_cup_mass_before_backfilling = "",
     sand_cup_mass_after_backfilling = "",
     tin_w_wet_sample= "",
     tin_w_OD_sample= "") %>%
     dplyr::arrange(.data$replication)

 mini_density_path <- here::here("ecmdata", "raw-data", "cleat-mark-testing", "other-data", date,
                        glue::glue("mini-density-data_{date}.csv"))

 readr::write_csv(mini_density_tibble, mini_density_path)


# cleat mark backfilling data sheet  --------------------------------------


 cleatmark_backfill_tibble <- tibble::tibble(
     experiment_name = experiment_name,
     date = date,
     cylinder_ID = c("01", "02", "03", "04", "05", "06", "07", "08","09", "10", "11", "12"),
     sand_loose_density = sand_loose_density,
     sand_cup_mass_before_backfilling = "",
     sand_cup_mass_after_backfilling = "",
     bowl_number = 1:12,
     bowl_tare = "",
     bowl_W_wet_sample = "",
     bowl_w_OD_sample = "")

 cleatmark_backfill_path <- here::here("ecmdata", "raw-data", "cleat-mark-testing",
                         date, glue::glue("cleat-mark-backfill-data_{date}.csv"))

 readr::write_csv(x = cleatmark_backfill_tibble, file = cleatmark_backfill_path)



# message returned if writing succeeds ------------------------------------

 written_files <- c(drydown_path, color_photos_path, penetrometer_path, mini_density_path, cleatmark_backfill_path)

if(all(file.exists(written_files))){
    message('Files written to disk:')
    crayon::blue(paste0(written_files, collapse = '\n'))
}

}
