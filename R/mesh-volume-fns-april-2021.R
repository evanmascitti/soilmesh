#' \lifecycle{experimental}
#'
#' @title Calculate soil volume above a reference plane
#'
#' @description One half of the total volume computation.
#'
#' @param mesh triangular mesh of class "mesh3d"
#' @param z_displ_mm vertical offset to prevent removal of vertices near the origin
#' @param direction one of "above" or "below"
#'
#' @return list containing a numeric vector of length 1 (cm3 of soil
#'   residing above the reference plane) and the resulting mesh from the slicing operation
#'
#' @details This applies a sort of hack to slice the mesh and keep only points
#'   residing above or below the x-y plane. The "hack" portion is translating the
#'   actual reference plane away from the mesh by a very small distance. The volume
#'   attributable to the offset is subtracted before completing the calculation.
#'   The meshes used in this project can _never_ be "watertight" and it is my
#'   hope that so long as the mesh is 2-manifold, the volume computation is
#'   accurate. This function calls [`Rvcg::vcgClean()`] to remove non-manifold
#'   faces, then remove unreferenced vertices, and (finally) remove non-manifold
#'   vertices before coherently orienting the faces. This approach seems to resolve the non-manifoldness of the meshes
#'   induced by downsampling, cylinder removal, and top/bottom removal.
#' @seealso [`vol_below()`]

#########
vol_one_side <-function(mesh, direction, z_displ_mm = 0.01){

  # slice the mesh along the x-y plane and keep only what's above OR below
  # as determined by the direction argument

  direction <- match.arg(direction, choices = c("above", "below"))

  sliced_mesh <- Morpho::cutMeshPlane(mesh = mesh,
                                      v1= c(1,0,0),
                                      v2= c(0,1,0),
                                      v3= c(-1,0,0),
                                      keep.upper = dplyr::if_else(
                                        direction == "above",
                                        true = TRUE,
                                        false = FALSE
                                      ))

  # invert normals of the portion beneath datum if measuring direction is below

  if(direction == "below"){
    sliced_mesh <- Morpho::invertFaces(sliced_mesh)
  }


  cap <- ref_circ

  if(direction == "below"){
    cap <- Morpho::invertFaces(cap)
  }

  cap <- rgl::translate3d(obj = soilmesh::ref_circ,
                          x= 0,
                          y= 0,
                          z= dplyr::if_else(
                            direction == "above",
                            -z_displ_mm,
                            z_displ_mm))

  # Updated 2021-04-30: some major changes:
  # - using uniform remeshing instead of ball pivoting
  # - coherently orienting faces during cleaning step
  # - inverting faces for vol_below so they are all facing outward
  # - updating normals after cleaning

  # browser()

  # z displacement of ref circ determined by direction argument
  # use ball pivoting
  newmesh <- Morpho::mergeMeshes(sliced_mesh, cap) %>%
    Rvcg::vcgClean(sel = c(2, 1, 4, 7, 2), iterate = T, silent = T) %>%
    Rvcg::vcgBallPivoting(radius = 0.005, deleteFaces = T) %>%
    Morpho::updateNormals()


  vol_value_mm3 <- Rvcg::vcgVolume(newmesh)

  # convert to cm3 and then subtract the extra volume added by offsetting the
  # reference plane....it is very small but just to be complete

  vol_diff_cm3 <- vol_value_mm3*0.001 - ((z_displ_mm*0.1)*186.2348)

  return(list(vol_diff_cm3 = vol_diff_cm3, sliced_mesh = newmesh))

}

  ############

#' Mesh volume above datum
#'
#' @inheritParams vol_one_side
#'
#' @export
vol_above <- function(mesh, z_displ_mm = 0.01){

 vol_one_side(mesh = mesh, direction = "above", z_displ_mm = z_displ_mm)

}

#' Mesh volume below datum
#'
#' @inheritParams vol_one_side
#'
#' @export
vol_below <- function(mesh, z_displ_mm = 0.01){

  vol_one_side(mesh = mesh, direction = "below", z_displ_mm = z_displ_mm)

}
