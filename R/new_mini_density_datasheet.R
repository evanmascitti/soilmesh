#' Populate empty data sheet for miniature sand-cone test
#'
#' A helper function wrapped by [`new_cleat_mark_datasheets()`]
#'
#' @param experiment_ID Character string identifying the experiment
#' @param date Date of data collection (YYYY-MM-DD)
#' @param tin_tare_set Character string identifying the date corresponding to
#'   the tin tares in **asi468**
#' @param sand_loose_density Bulk density of sand used for sand-cone test,
#'   defaults to 1.54 as was measured in Dec. 2020 for the topdressing sand
#'   obtained from Valentine research center.
#'
#' @export
#'
#' @return silently writes file to disk
#'
new_mini_density_datasheet <- function(experiment_ID, date, tin_tare_set,
                                       sand_loose_density = 1.54){
  mini_density_tibble <- tidyr::crossing(
    experiment_ID = experiment_ID,
    date = date,
    cylinder_ID = c("01", "02", "03", "04", "05", "06", "07", "08","09", "10", "11", "12"),
    replication = 1:3,
    tin_tare_set = tin_tare_set,
    tin_number = "",
    sand_cup_mass_before_backfilling = "",
    sand_cup_mass_after_backfilling = "",
    tin_w_wet_sample= "",
    tin_w_OD_sample= "")

  filepath <- paste0(here::here("analysis", "data", "raw_data", "cleat_mark_testing",
                         date),
                     "/",
                     date, "_mini_density_data.csv")

  readr::write_csv(mini_density_tibble, filepath)

}
