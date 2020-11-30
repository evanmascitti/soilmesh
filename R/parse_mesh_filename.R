#' `r lifecycle::badge('experimental')`
#'
#' @title Split the base file name of a mesh scan into atomic idenifiers
#'
#' @description A file naming sheme was developed for this project to ensure all
#'   data files are uniquely identified. Each `.ply` file has a unique
#'   identifier containing 9 sepearate pieces of information (see Details). This
#'   function parses the file name into useful metadata about the specimen it
#'   records, and the resulting tibble can be used for anaysis with
#'   **tidyverse** tools.
#'
#' @details Each file name contains 9 pieces of information, separated by
#'   underscores. They are (in order of appearance):
#'
#'   1. `inf_mix_number` Overall 3-digit mix number for the main series of
#'   experiments
#'   2. `sand_pct` Percent sand-size particles (i.e. > 53 &mu; sieve
#'   diameter) by oven-dry mass (two digits). Encoded in file name as a percent
#'   (i.e. 55) but converted to a decimal after parsing.
#'   3. `sand_name` Unique
#'   camelCase character string which identifies the source of sand, i.e.
#'   "boydMedium"
#'   4. `clay_name` Unique camelCase character string which identifies the
#'   source of fine-grained soil, i.e. "brownGumbo"
#'   5. `rep` Replicate ID for a given mix (A-C)
#'   6. `run` Successive date of testing
#'   (1-n)
#'   7. `cylinder_ID` The unique number on the aluminum specimen mold used
#'   for the sample (1-12)
#'   8. `overall_scan_number` Unique 4-digit
#'   9. `dttm` Calendar date-time of the scan in POSIX format, i.e.
#'   YYYY-MM-DD-HH-MM
#'
#' Note that the percent "clay" (i.e. fines) is not included as an element.
#' Including this value would be redundant because its value is implicitly
#' encoded by the sand %; the "clay %" i.e.  is simply 1 - sand_pct.
#'
#' @param df Data frame containing two columns, 'mesh_file_name', and 'ply_file'
#'
#' @return Data frame containing 11 columns, 10 of which are information about
#'   the scanned specimen and 1 of which is a list-column holding the `"mesh3d'`
#'   objects.
#' @export
#'
#' @examples
parse_mesh_filename <- function(df){
  tidyr::separate(
    data = df,
    col = .data$mesh_file_name,
    into = c("inf_mix_number", "sand_pct", "sand_name", "clay_name", "rep", "run", "cylinder_ID", "overall_scan_number", "dttm"),
    sep = "_",
    convert = TRUE
  ) %>%
    mutate(sand_pct = sand_pct / 100)
}
