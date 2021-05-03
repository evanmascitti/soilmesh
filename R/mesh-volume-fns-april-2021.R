utils::globalVariables('ref_circ')

#' \lifecycle{experimental}
#'
#' @title Calculate soil volume above a reference plane
#'
#' @description One half of the total volume computation.
#'
#' @param mesh triangular mesh of class "mesh3d"
#' @param z_displ_mm vertical offset to prevent removal of vertices near the origin
#' @param direction one of "above" or "below"
#' @param include_ref whether the returned mesh object should include the
#'   reference circle plane? Default is `FALSE which` returns only the points
#'   retained after slicing.
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
#' @export
#' @seealso [`vol_below()`], [`vol_above()`], [`vol_diff()`]
#'
#########
vol_slice <-function(mesh, direction, z_displ_mm = 0.01, include_ref = FALSE){

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

  # browser()

  if(!include_ref) {
    return_mesh <- sliced_mesh
  } else {
    return_mesh <- newmesh
    }

  return(list(vol_diff_cm3 = vol_diff_cm3,
              sliced_mesh = return_mesh)
  )

}

  ############

#' Mesh volume above datum
#'
#' @inheritParams vol_slice
#'
#' @export
vol_above <- function(mesh, z_displ_mm = 0.01){

 vol_slice(mesh = mesh, direction = "above", z_displ_mm = z_displ_mm)

}

#' Mesh volume below datum
#'
#' @inheritParams vol_slice
#'
#' @export
vol_below <- function(mesh, z_displ_mm = 0.01){

  vol_slice(mesh = mesh, direction = "below", z_displ_mm = z_displ_mm)

}


#' Calculate above, below, and summation of these
#'
#' Returns a list with each named element being a numeric vector
#' of length 1
#'
#' @return named list of length 3
#' @export
#'
#' @inheritParams vol_slice
vol_diff <- function(mesh, z_displ_mm = 0.01) {

  remeshed_vol_above <- vol_above(mesh)$vol_diff_cm3
  remeshed_vol_below <- vol_below(mesh)$vol_diff_cm3
  remeshed_vol_total <- sum(remeshed_vol_below, remeshed_vol_above)

  return_list <- mget(x = c('remeshed_vol_above',
                            'remeshed_vol_below',
                            'remeshed_vol_total'),
                      envir = rlang::current_env(),
                      inherits = F)

  return(return_list)

}
