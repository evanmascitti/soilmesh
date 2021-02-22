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
#' @export
#'@seealso [`ecmfuns::gather.files()`], [`ecmfuns::read.files()`], [`Rvcg::vcgImport()`]


read_meshfiles <- function(dir = NULL, ...) {

  if(missing(dir)){
    stop("\nNo directory specified, please indicate where the `.ply` files are stored.")
  }

  paths <- list.files(path= dir, pattern = "[.]ply$", recursive = FALSE, full.names = TRUE)

  file_names <- stringr::str_remove(string = basename(paths), pattern = "[.]ply$")

  if(length(paths) == 0){
    stop("No .ply files found in directory, did you search in the correct place?")
  }

  ply_files <-purrr::map(
    .x= paths,
    .f= ~Rvcg::vcgImport(...) ) %>%
    purrr::set_names(nm = file_names) %>%
    tibble::enframe(name = "mesh_file_basename", value= "mesh_object")

  return(ply_files)

}
