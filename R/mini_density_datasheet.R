#' Populate empty data sheet for miniature sand-cone test
#'
#' Eventually this should just be a helper function wrapped by [`cleat_mark_datasheets()`]
#'
#' @param experiment_name Character string identifying the experiment
#' @param date Date of data collection (YYYY-MM-DD)
#' @param tin_tare_set Character string identifying the date corresponding to
#'   the tin tares in **asi468**
#' @param sand_loose_density Bulk density of sand used for sand-cone test,
#'   defaults to `asi468::vrc_sand_loose_density`, which is a data object stored
#'   in the asi468 package. Its value is 1.54, as was measured in Dec. 2020 for
#'   the topdressing sand obtained from Valentine research center.
#'
#'
#' @return silently writes file to disk
#'
mini_density_datasheet <- function(experiment_name, date, tin_tare_set, n_reps = 1,
                                       sand_loose_density = asi468::vrc_sand_loose_density){
  mini_density_tibble <- tidyr::crossing(
    experiment_name = experiment_name,
    date = date,
    cylinder_ID = c("01", "02", "03", "04", "05", "06", "07", "08","09", "10", "11", "12"),
    replication = 1:n_reps,
    sand_loose_density = sand_loose_density,
    tin_tare_set = tin_tare_set,
    tin_number = "",
    sand_cup_mass_before_backfilling = "",
    sand_cup_mass_after_backfilling = "",
    tin_w_wet_sample= "",
    tin_w_OD_sample= "") %>%
    dplyr::arrange(.data$replication)

  filepath <- here::here("ecmdata", "raw-data", "cleat-mark-testing", date,
                                glue::glue("mini-density-data_{date}.csv"))

  readr::write_csv(mini_density_tibble, filepath)

}
