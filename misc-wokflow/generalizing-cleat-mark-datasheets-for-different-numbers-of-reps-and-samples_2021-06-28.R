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

  # browser()


  # substitute in the value for tin_tare_set and tin_numbers
  # if supplied by user; otherwise default to an empty string

  tin_tare_label <- tin_tare_set %||% " "

  drydown_tin_numbers_vector <- drydown_tin_numbers %||% " "

  mini_density_tin_numbers_vector <- mini_density_tin_numbers %||% " "

  bowl_tare_set_label <- bowl_tare_set %||% " "


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
    stop("The following directory(s) already exist: \n\n", cat(new_dirs[dir.exists(new_dirs)], fill = T), "\n\n",
         "Halting function call to prevent over-write.", call. = T)
  }

  # construct file paths for writing and store in a list

  other_paths <- ecmfuns::build_datasheet_path(base = here::here("ecmdata/raw-data/cleat-mark-testing", date, "other-data"),
                                               stem = c('drydown-data', "cleat-mark-backfill-data", "mini-density-data", 'penetrometer-data'),
                                               date = date,
                                               ext = "csv")

  photos_path <- ecmfuns::build_datasheet_path(base = here::here("ecmdata/raw-data/cleat-mark-testing", date, "color-photos"),
                                               stem = c('color-photos-index'),
                                               date = date,
                                               ext = "csv")

  # construct tibbles to write  ---------------------------------------------


  # drydown tibble

  drydown_data_tibble <- tibble::tibble(
    experiment_name = experiment_name,
    date = date,
    sample_name = dplyr::if_else(
      condition =  length(sample_names)  == 4L,
      true = rep(rep(sample_names, each = 3), times=2),
      false = sample_names
    ),
    cylinder_ID = dplyr::if_else(
      length(sample_names)  == 4L,
      true = rep(c(paste0(0, 1:9), 10:12), times=2),
      false = paste0(0, 1:length(sample_names))
    ),
    time_type = rep(c("lamp_on", "test_time"),
                    each= dplyr::if_else(
                      condition = length(sample_names) ==4L,
                      true = 12,
                      false = length(sample_names))),
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
    test_order = length(sample_names),
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


  # Since they are in different sub-directories, I can't use `sort`
  # to order the names of the tibbles and also the file paths.

  # Therefore the actions below are done once for each directory.
  # In this case it is only a few actions, each copy-pasted once so it's OK.

  # construct lists containing the data frames to write

  other_data_sheets <- mget(ls(pattern = "data_tibble", sorted = T),
                            mode = "list", inherits = F)

  color_photos_index_sheet <- mget(ls(pattern = "color_photos_index_tibble", sorted = T),
                                   mode = "list", inherits = F)



  # put them together into a tibble containing
  # the path to write and a list-column of corresponding data frames

  other_args <- tibble::tibble(
    x = other_data_sheets,
    file = sort(other_paths)
  )

  # Since this is only one sheet it could technically be done
  # with a single call to `readr::write_csv` but for consistency
  # I will still make it into a named list and call `pwalk()`as above

  color_photos_args <- tibble::tibble(
    x = color_photos_index_sheet,
    file = sort(photos_path)
  )




  ##############


  # create new directories first
  purrr::walk(new_dirs, dir.create)

  # write the files to disk
  purrr::pwalk(other_args, readr::write_csv)
  purrr::pwalk(color_photos_args, readr::write_csv)


  ##############



  #  browser()
  # message returned if writing succeeds ------------------------------------

  n_files <- sum(purrr::map_dbl(c(other_paths, photos_path), length))

  if(all(file.exists(other_paths, photos_path))){
    message(crayon::green(n_files, " files were written to disk."))
  }



}
