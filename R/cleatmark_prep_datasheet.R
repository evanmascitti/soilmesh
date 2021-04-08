#' Populate a Blank csv File for Quality Control
#'
#' Simplifies the process of measuring dry density and water content
#' of compacted specimens. Dry density should correspond closely to
#' that achieved with Proctor testing. Column names are mostly compatible
#' with [soiltestr::add_physical_properties()]; the exception is that
#' the properties of the cylinders themselves (stored in [`chunk_cyl_dims`] have different names to make it clear that their
#' masses and volumes include the foam plugs). These are automatically
#' converted by [soiltestr::add_physical_properties()].
#'
#' @param dir Character. Directory to which file should be written.
#' @param date Character. Date specimens were prepared; ISO format (YYYY-MM-DD)
#' @param tin_tare_set Character. Identifier corresponding to the set of tins used to measure water content. See [asi468::tin_tares].
#' @return Writes file to disk and prints a message.
#' @export
#'
cleatmark_prep_datasheet <- function(dir, date, tin_tare_set = "") {

df <- tibble::tibble(
  date = date,
  experiment_name = "",
  sample_name	="",
  cylinder_ID	= c(paste0(0, 1:9), 10:12),
  filled_cylinder_mass_g = "",
  tin_tare_set = tin_tare_set,
  tin_number = "",
  tin_w_wet_sample = "",
  tin_w_OD_sample = "",
  comments = ""
)

file_path <- here::here(dir, glue::glue("cleatmark-cyl-prep-QC_{date}.csv"))

if(file.exists(file_path)) {
  stop("File already exists; stopping call to prevent over-write")
} else {
  readr::write_csv(df, file_path)
}

if(file.exists(file_path)){
  crayon::green(glue::glue("Success! Wrote {file_path} to disk."))
}

}
