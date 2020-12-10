#' \lifecycle{experimental}
#'
#' @title Compute disturbed soil volume **below** original surface plane
#'
#' @description Slice the mesh at the z=0 plane and keep only points at or above
#'   the plane. Then merge it with the reference plane of the same diameter.
#'   Compute the volume in mm3.
#'
#' @param mesh triangular mesh of class "mesh3d"
#' @param show whether to display the sliced mesh in a new **rgl** window.
#'
#' @return mesh volume in mm3
#' @importFrom magrittr `%T>%`
#' @seealso [`vol_above()`], [`vol_below()`]
#' @export
vol_below <- function(mesh, show = FALSE){

  # slice the mesh along the x-y plane and keep only what's above
  # use the rounded ref circle as the base

  if(show == TRUE){
    Morpho::mergeMeshes(mesh, soilmesh::ref_circ) %>%
    Morpho::cutMeshPlane(v1= c(1,0,0),
                         v2= c(0,1,0),
                         v3= c(-1,0,0),
                         keep.upper = FALSE) %>%
    Rvcg::vcgBallPivoting() %>%
    Rvcg::vcgClean(sel = c(0:6)) %>%
    soilmesh::remove_cyl() %>%
    rgl::shade3d %T>%
    Rvcg::vcgClean(sel = c(0:6), iterate = T) %>%
    Rvcg::vcgVolume()
  }

  if(show == FALSE){
    Morpho::mergeMeshes(mesh, soilmesh::ref_circ) %>%
      Morpho::cutMeshPlane(v1= c(1,0,0),
                           v2= c(0,1,0),
                           v3= c(-1,0,0),
                           keep.upper = FALSE) %>%
      Rvcg::vcgBallPivoting() %>%
      Rvcg::vcgClean(sel = c(0:6)) %>%
      soilmesh::remove_cyl() %>%
      Rvcg::vcgClean(sel = c(0:6), iterate = T) %>%
      Rvcg::vcgVolume()
  }

}


