#' Show an overhead view of a mesh, with a square viewport and better lights
#'
#' @return alters existing rgl scene
#' @export
#'
overhead_mesh_view <- function() {
    rgl::plot3d(overhead_mesh_scene)
  }



#' Save an overhead snapshot of a mesh and trim the white space
#'
#' @param mesh mesh3d object to screenshot
#' @param file path to save the screenshot
#' @param mesh_path optionally allows mesh to be snapshotted directly from a file path. If `mesh` is provided, this argument is ignored
#' @param ... arguments passed to `rgl::shade3d()`
#' @return Writes file to disk
#' @export
#'
overhead_mesh_snapshot <- function(mesh = NULL, file, mesh_path = NULL, ...){

  # if the mesh has not yet been read, sub in the path and read it

  mesh <- mesh %||% Rvcg::vcgImport(mesh_path)


  # configure scene
  overhead_mesh_view()

  rgl::shade3d(mesh, ...)

  # take snapshot and save to temporary file
  temp_path <- tempfile()

  rgl::rgl.snapshot(filename = temp_path, fmt = 'png')

  rgl::rgl.close()

  # trim whitespace with magick and write to disk

  trimmmed <- magick::image_read(temp_path) %>%
    magick::image_trim() %>%
    magick::image_write(file, format = 'png')

  invisible({file.remove(temp_path)})

  }
