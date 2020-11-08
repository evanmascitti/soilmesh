#' \lifecycle{experimental}
#'
#' Remove the specimen container from an oriented 3D mesh
#'
#' Boolean mesh operation to keep all points inside or outside the clipping surface
#'
#' @param mesh The "mesh3d" object to trim
#' @param radius Radius of clipping surface in mm
#' @param clip_fun A function corresponding to the bounding surface, defaults to an open cylinder
#' @param keep "inside" for vertices contained by the surface; "outside" for those falling beyond it
#' @param ... allows user to set the `attribute` argument if needed (see \link[rgl]{clipMesh3d})
#'
#' @return A "mesh3d" object with undesired vertices removed
#' @export
#'
#' @example inst/examples/remove_cyl_example.r
remove_cyl <- function(mesh, radius = 70, clip_fun = "default", keep = "inside", ...) {
  if(radius != 70){
    if(usethis::ui_yeah(x= "You have specified a cylinder dimension other than the default. Did you mean to do this?", n_yes = 1, n_no = 1, shuffle = FALSE) == FALSE){
      stop("\nOK you dummy, call the function again without changing the default radius!")
    }
  }

  boundary_function <- function(m){m[,1]^2 + m[,2]^2}

  if (clip_fun == "default"){
   clipping_function <- function(m){m[,1]^2 + m[,2]^2} } else {
   clipping_function <- clip_fun}

  if (keep == "inside") {
    clipped_mesh <- rgl::clipMesh3d(mesh = mesh, fn = clipping_function, bound = radius^2, greater = FALSE) }
    else if (keep == "outside") {
      clipped_mesh <- rgl::clipMesh3d(mesh = mesh, fn = clipping_function, bound = radius^2, greater = TRUE)
    } else{
      stop("\nYou have to keep either the inside or the outside of the mesh! Pick one and check your spelling!")
    }

  return(clipped_mesh)
}
