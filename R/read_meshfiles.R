#' @title Read multiple 3D mesh files
#'
#' @description A specific function to automate reading of `.ply` files into R. Specify a
#' directory containing the files to automatically read all contained `.ply`
#' files into R and compile them into a nested tibble with one row per file.
#' Result can be piped to [`parse_mesh_filename()`] prior to mesh
#' processing/analysis.
#'
#' @param dir Directory in which to search for `.ply` files
#' @param ... additional arguments passed to [`Rvcg::vcgImport()`]
#'
#'@details Wraps [`list.files()`] from base R, [`stringr::str_remove()`],
#'  [`purrr::map()`], and [`Rvcg::vcgImport()`] with specific arguments to
#'  locate files, import to current session , strip file extensions, and compile
#'  in tidy format.
#'
#'@seealso [`ecmfuns::gather.files()`], [`ecmfuns::read.files()`], [`Rvcg::vcgImport()`]


read_meshfiles <- function(dir = NULL, ...) {

  if(missing(dir)){
    stop("\nNo directory specified, please indicate where the `.ply` files are stored.")
  }

  paths <- list.files(path= dir, pattern = ".ply", recursive = F)

  if(length(paths) == 0){
    stop("No .ply files found in directory, did you search in the correct place?")
  }

  plyfiles <-purrr::map(
    .x= paths,
    .f= ~Rvcg::vcgImport(...) ) %>%
    tibble::enframe(name = "mesh_file_name", value= "ply_file")

  return(plyfiles)

}
