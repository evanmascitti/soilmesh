#' Populate empty data sheet for filling in cleats with sand
#'
#' @inheritParams mini_density_datasheet
#'
#' @return writes file to disk
#' @export
#'
cleat_mark_backfill_datasheet <- function(experiment_name, date, tin_tare_set,
                                     sand_loose_density = asi468::vrc_sand_loose_density){

  cleatmark_backfill_tibble <- tibble::tibble(
    experiment_name = experiment_name,
    date = date,
    cylinder_ID = c("01", "02", "03", "04", "05", "06", "07", "08","09", "10", "11", "12"),
    sand_loose_density = sand_loose_density,
    sand_cup_mass_before_backfilling = "",
    sand_cup_mass_after_backfilling = "")

  file_path <- here::here("ecmdata", "raw-data", "cleat-mark-testing",
                          date, glue::glue("cleat-mark-backfill-data_{date}.csv"))

  readr::write_csv(x = cleatmark_backfill_tibble, file = file_path)

}
