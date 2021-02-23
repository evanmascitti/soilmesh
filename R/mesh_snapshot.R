#' @title Save a snapshot of a 3D soil mesh
#'
#' @description Optional aspect ratio and extra components.
#'
#' @param mesh "mesh3d" object
#' @param outfile path to save the snapshot (png format)
#' @param wire whether to render mesh edges as a wire frame
#' @param axes whether to also plot x, y, and z axes
#' @param aspect_ratio either "16x9" (for widescreen presentation) or "4x3"
#' @param overwrite whether to force over-write existing files of same names
#'
#' @return
#' @export
#'
mesh_snapshot <- function(mesh, outfile, wire = TRUE, axes = FALSE,
                          aspect_ratio = "16x9",  overwrite = FALSE){

  # error messages if arguments mis-specified

# browser()

   if(! aspect_ratio %in% c("16x9", "4x3")){
    stop("\nOnly 16x9 and 4x3 aspect ratios are supported.
      Please specify one of these.")
  }

  # append outfile argument to contain aspect ratio and if the file extension is missing, add this as well

  if(!stringr::str_detect(outfile, "[.]png$")){
outfile <- paste0(outfile, "_", aspect_ratio, ".png")
  } else{
    outfile <- stringr::str_replace(string = outfile,
                                    pattern = "[.]png$",
                                    replacement = paste0("_", aspect_ratio, ".png"))
  }

  # stop if the outfile already exists
  if(file.exists(outfile) && overwrite == FALSE){
    stop("\nFile already exists. Use `overwite = TRUE` to over-write existing file.")
  }
  # reset viewer to default parameters


  rgl::clear3d(type = c("shapes", "lights"))
  suppressWarnings(rgl::par3d(r3dDefaults) )
  rgl::rgl.viewpoint()
  rgl::bg3d("transparent")



  # set parameters for call to `par3d()`

  if(aspect_ratio == "16x9"){
    snapshot_parameters <- soilmesh::rgl_params_16x9
  }

  if(aspect_ratio == "4x3"){
    snapshot_parameters <-soilmesh::rgl_params_4x3
  }

  # set other desired background parameters for viewer window
  rgl::bg3d("grey80")
  rgl::rgl.light(phi = 60, specular = grDevices::grey.colors(8)[2])
  rgl::par3d(snapshot_parameters)

  # render mesh

  rgl::shade3d(mesh)

  if(wire){
    rgl::wire3d(mesh, color = 'grey20')
  }

  if(axes){
    soilmesh::add_origin_axes()
  }

  rgl::rgl.snapshot(filename = outfile, fmt = "png")

  rgl::rgl.close()
}


