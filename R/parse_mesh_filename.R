#' `r lifecycle::badge('experimental')`
#'
#' @title Split the base file name of a mesh scan into atomic idenifiers
#'
#' @description A file naming sheme was developed for this project to ensure all
#'   data files are uniquely identified. Each `.ply` file contains 4 pieces of
#'   information (see details). This function parses the file name into useful
#'   metadata about the specimen. Returns a tibble for use subsequent analysis
#'   with **tidyverse** tools.
#'
#' @details Each file name contains 4 pieces of information, separated by
#'   underscores. These provide enough information to uniquely identify every
#'   scan, and the additional information about the soil can be added via
#'   joining other data frames. The pieces of information encoded in the file
#'   names are (in order of appearance):
#'
#'   1. `experiment_name`: A two-digit identifier or a character string
#'   corresponding to the particular set of mixes or task. 2. `soil_ID` 3-digit
#'   number or other character string identifying the soil _within_ the
#'   experiment 3. `date` Calendar date of the scan in POSIX format, i.e.
#'   YYYY-MM-DD 4. `cylinder_ID` The aluminum sample holder containing the
#'   specimen of interest
#'
#'
#' @param x Character vector of `.ply` files to read
#'
#' @return Data frame containing 5 columns; the first holds the full file path
#' while the rest hold the data which uniquely identify the file (See details)
#' @export
#'
parse_mesh_filename <- function(x){

  mesh_file_info <- tibble::tibble(
    full_path = x,
    sans_extension = stringr::str_remove_all(string = basename(.data$full_path),
                                         pattern = "[.]ply$|_processed")) %>%
    tidyr::separate(
      col = .data$sans_extension,
      into = c("experiment_name", "soil_ID", "date", "cylinder_ID"),
      sep = "_",
      convert = FALSE,
      remove = TRUE
      ) %>%
    dplyr::mutate(cylinder_ID = stringr::str_remove(string = .data$cylinder_ID,
                                                    pattern = "cyl"),
                  date = lubridate::as_date(date)
    )

  return(mesh_file_info)
}
