#' @title Read multiple 3D mesh files
#'
#' @description A wrapper to automate reading of `.ply` files into R. Specify a
#'   character vector of file paths which are compiled into a nested tibble with
#'   one row per file. The names of the files are parsed with
#'   [`parse_mesh_filename()`] to include the minimum amount of information
#'   required to uniquely identify the mesh. Other data such as water content
#'   can be added via joins to other data frames.
#'   )
#'
#' @param x character vector of paths pointing to .ply files
#' @param colors whether to read mesh colors (defaults to TRUE)
#' @param ... additional arguments passed to [`Rvcg::vcgImport()`]
#' @export
#' @seealso [`Rvcg::vcgImport()`], [`parse_mesh_filename()`]


read_meshfiles <- function(x, colors = TRUE, ...) {

  meshes <- soilmesh::parse_mesh_filename(x) %>%
    dplyr::group_by(.data$full_path) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      mesh_object = purrr::map(
        .x = .data$full_path,
        .f = Rvcg::vcgImport,
        readcolor = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(.data$data) %>%
    dplyr::select(-.data$full_path)

  return(meshes)

}
