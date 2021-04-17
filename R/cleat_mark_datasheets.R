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
#' @param tin_tare_set Optional character string (length 1) corresponding to set of tin tares used for weighing soil
#' @param drydown_tin_numbers Optional character vector corresponding (length 24) to specific tin tare numbers used for weighing soil in drydown comparison
#' @param mini_density_tin_numbers Optional character vector corresponding (length 12) to specific tin tare numbers used for weighing soil in mini-density measurements
#'
#' @return Writes new folders and empty data files to disk.
#'
#' @export
#'
#' @importFrom rlang `%||%`
#'
cleat_mark_datasheets <- function(experiment_name, sample_names, date, tin_tare_set = NULL,
                                      bowl_tare_set = NULL, sand_loose_density = asi468::vrc_sand_loose_density,
                                      mini_density_reps = 1, drydown_tin_numbers = NULL,
                                      mini_density_tin_numbers = NULL){


  # substitute in the value for tin_tare_set and tin_numbers
  # if supplied by user; otherwise default to an empty string

  tin_tare_label <- tin_tare_set %||% ""

  drydown_tin_numbers_vector <- drydown_tin_numbers %||% ""

  mini_density_tin_numbers_vector <- mini_density_tin_numbers %||% ""

  bowl_tare_set_label <- bowl_tare_set %||% ""


  # build paths to three new directories: one for everything that will be
  # collected today, and sub-directories under this one for the raw meshes and the
  # photos. The mesh files will be manually copied into this directory at the end
  # of the day, either with point-and-click or potentially using a helper function
  # which I haven't yet written.


  parent_dir <- here::here("ecmdata/raw-data/cleat-mark-testing", date)

  sub_dirs <- paste(parent_dir, c("raw-meshes", "other-data", "color-photos"), sep = "/")

  new_dirs <- c(parent_dir, sub_dirs)


  # check if directories already exist, but don't write them until later

  if(any(dir.exists(paths = new_dirs))){
    stop("The following directory(s) already exist: \n", cat(new_dirs[dir.exists(new_dirs)], fill = T), "\n",
         "Halting function call to prevent over-write.", call. = T)
  }

  # construct file paths for writing and store in a list

  other_paths <- ecmfuns::build_datasheet_path(base = here::here("ecmdata/raw-data/cleat-mark-testing", date, "other-data"),
                                               stem = c('drydown-data', "cleat-mark-backfill-data", "mini-density-data", 'penetrometer-data'),
                                               date = Sys.Date(),
                                               ext = "csv")

  photos_path <- ecmfuns::build_datasheet_path(base = here::here("ecmdata/raw-data/cleat-mark-testing", date, "color-photos"),
                                               stem = c('color-photos-index'),
                                               date = Sys.Date(),
                                               ext = "csv")

  files_to_write <- c(photos_path, other_paths)




  # construct tibbles to write  ---------------------------------------------


  # drydown tibble

  drydown_data_tibble <- tibble::tibble(
    experiment_name = experiment_name,
    date = date,
    sample_name = rep(rep(sample_names, each = 3), times=2),
    cylinder_ID = rep(1:12, times=2),
    time_type = rep(c("lamp_on", "test_time"), each=12),
    time = "",
    AM_PM = "",
    tin_tare_set = tin_tare_label,
    tin_number = drydown_tin_numbers_vector,
    tin_w_wet_sample= "",
    tin_w_OD_sample = "",
    comments = "-"
  )

  # color_photos index  ------------------------------------------------------------


  # this empty file will keep track of the order of the images I shot on my phone;
  # this way I don't need to know their file names/numbers and I can test
  # the cylinders out of order if needed

  color_photos_index_tibble <- tibble::tibble(
    experiment_name = experiment_name,
    date = date,
    test_order = 1:12,
    cylinder_ID = "")

  # penetrometer data  ------------------------------------------------------------

  penetrometer_data_tibble <- tidyr::crossing(
    experiment_name = experiment_name,
    date = date,
    cylinder_ID = c(paste0(0, 1:9), 10:12),
    replication = 1:4,
    penetrometer_reading = ""
  ) %>%
    dplyr::arrange(.data$cylinder_ID)

  # mini-density data -------------------------------------------------------

  mini_density_data_tibble <- tidyr::crossing(
    experiment_name = experiment_name,
    date = date,
    cylinder_ID = c(paste0(0, 1:9), 10:12),
    replication = 1:mini_density_reps,
    sand_loose_density = sand_loose_density,
    tin_tare_set = tin_tare_label,
    tin_number = mini_density_tin_numbers_vector,
    sand_cup_mass_before_backfilling = "",
    sand_cup_mass_after_backfilling = "",
    tin_w_wet_sample= "",
    tin_w_OD_sample= "") %>%
    dplyr::arrange(.data$cylinder_ID, .data$replication)

  # cleat mark backfilling data sheet  --------------------------------------

  cleat_mark_backfill_data_tibble <- tibble::tibble(
    experiment_name = experiment_name,
    date = date,
    cylinder_ID = c(paste0(0, 1:9), 10:12),
    sand_loose_density = sand_loose_density,
    sand_cup_mass_before_backfilling = "",
    sand_cup_mass_after_backfilling = "",
    bowl_tare_set = bowl_tare_set_label,
    bowl_number = 1:12,
    bowl_w_wet_sample = "",
    bowl_w_OD_sample = "")

  # compile objects and names before writing  -------------------------------


  # construct list containing the data frames to write

  data_sheets <- mget(ls(pattern = "tibble", sorted = T),
                      mode = "list", inherits = F)

  # put them together into a tibble containing
  # the path to write anda list-column of corresponding data frames

  args <- tibble::tibble(
    x = data_sheets,
    file = sort(files_to_write)
  )

  ##############
  # browser()

  # create new directories first
  purrr::walk(new_dirs, dir.create)

  # write the files to disk
  purrr::pwalk(args, readr::write_csv)


  ##############




  # message returned if writing succeeds ------------------------------------

  if(all(file.exists(files_to_write))){
    glue::glue("{length(files_to_write)} files were written to disk.")
   # cat(files_to_write)
  }



}
