# there are 4 total methods in here; a couple have multiple functions associated
# with them (i.e. helpers and wrappers)

# the basic approaches is to take a point and compare it to the reference datum,
# them multiply by cross sectional area to estimate the volume of disturbed
# soil relative to a purely planar surface.

# the four approaches take these "flavors":
# - compute a grid and use those points (using Morpho::MeshDist)
# - use every point on the 'target' mesh i.e. the soil specimen tested and scanned
# - use every _face_ on the 'target' mesh i.e. the soil specimen tested and scanned;
#   and calculate the distance for all 3 points....then take the average of these as
#   the representative  value for the distance of that face, and multiply that distance
#   by the area of the face to give a truer estimation of the volume the "column" of soil
#   is occupying
# - split the mesh into 2 halves, then combine it with the actual reference circle
#   into a new mesh, and compute the volume of it with vcgVolume


utils::globalVariables('ref_circ')

#' \lifecycle{experimental}
#'
#' @title Calculate soil volume above or below a reference plane
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
#' @seealso [`vol_gridded()`], [`vol_faceweighted()`], [`vol_pointwise()`]
#'
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

  cap <- rgl::translate3d(obj = ref_circ,
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


#' \lifecycle{experimental}
#' Mesh volume above datum
#'
#' Returns sliced mesh and volume computed by re-meshing method
#'
#' @inheritParams vol_slice
#'
#' @export
vol_slice_above <- function(mesh, z_displ_mm = 0.01){

 vol_slice(mesh = mesh, direction = "above", z_displ_mm = z_displ_mm)

}

#' \lifecycle{experimental}
#' Mesh volume below datum
#'
#' Returns sliced mesh and volume computed by re-meshing method
#'
#' @inheritParams vol_slice
#'
#' @export
vol_slice_below <- function(mesh, z_displ_mm = 0.01){

  vol_slice(mesh = mesh, direction = "below", z_displ_mm = z_displ_mm)

}

#' \lifecycle{experimental}
#' Calculate above, below, and summation of these
#'
#' Returns a list with each named element being a numeric vector
#' of length 1
#'
#' @return named list of length 3
#' @export
#'
#' @inheritParams vol_slice
vol_remeshed <- function(mesh, z_displ_mm = 0.01) {

  remeshed_vol_above <- vol_slice_above(mesh)$vol_diff_cm3
  remeshed_vol_below <- vol_slice_below(mesh)$vol_diff_cm3
  remeshed_vol_total <- sum(remeshed_vol_below, remeshed_vol_above)

  return_list <- mget(x = c('remeshed_vol_above',
                            'remeshed_vol_below',
                            'remeshed_vol_total'),
                      envir = rlang::current_env(),
                      inherits = F)

  return(return_list)

}


# new section -------------------------------------------------------------



# This is a similar method to what I wrote in python, except it
# computes a signed distance field using the MeshDist class
# in Morpho.

# Not sure how this will compare to the other things I have calculated
# but it is worth a try - I think basically it voxelizes all the points
# into a 3D grid and computes the distance to the surface for each one.

# xfun::pkg_attach(c('Morpho', 'purrr', 'Rvcg', 'rgl'))

#' \lifecycle{experimental}
#' Compute volume difference based on signed distance field
#'
#' Uses number of points above and below datum to compute a weighted
#' average of the soil volume above and below datum
#'
#' @param mesh object of class "mesh3d"
#'
#' @return list of length 3 (above, below, total volumes in cm^3^)
#' @export
#'
vol_gridded <- function(mesh){

  md_obj <- Morpho::meshDist(x = ref_circ,
                             mesh2 = mesh,
                             plot = F)

  distances <- md_obj$dists

  n_pts <- length(distances)

  pos_dists <- distances[distances > 0]

  neg_dists <- distances[distances <= 0]

  avg_pos_dist_cm <- 0.1 * sum(pos_dists) / n_pts

  avg_neg_dist_cm <- 0.1 * sum(neg_dists) / n_pts

  area_cm2 <- pi * 7 ^ 2

  gridded_vol_above <- avg_pos_dist_cm * area_cm2

  gridded_vol_below <- abs(avg_neg_dist_cm * area_cm2 )

  gridded_vol_total <- sum(gridded_vol_above, gridded_vol_below)

  return_list <- mget(x = c('gridded_vol_above',
                            'gridded_vol_below',
                            'gridded_vol_total'),
                      envir = rlang::current_env(),
                      inherits = F)

  return(return_list)
}


# measure for every point on the mesh -------------------------------------

#' \lifecycle{experimental}
#' Compute volume from distances of every point on the mesh
#'
#' Uses number of points above and below datum to compute a weighted
#' average of the soil volume above and below datum
#'
#' @inheritParams vol_gridded
#'
#' @return list of length 3 (above, below, total volumes in cm^3^)
#' @export
#'
vol_pointwise <- function(mesh){

  # note that this is the same basic approach as that taken
  # for the vol_gridded function and most of the code is simply
  # copy-pasted from that function. Not worth taking the time
  # to generalize this.
  # The main differnece is how the distances object (an atomic
  # vector of all the distance values) is created....in this
  # function it is from _all_ points on the target mesh
  # instead of a sampled grid. Both retain their signs.

  # browser()

  clost_result <- Rvcg::vcgClost(x= ref_circ, mesh = mesh)

  distances <- clost_result$quality

  n_pts <- length(distances)

  pos_dists <- distances[distances > 0]

  neg_dists <- distances[distances <= 0]

  avg_pos_dist_cm <- 0.1 * sum(pos_dists) / n_pts

  avg_neg_dist_cm <- 0.1 * sum(neg_dists) / n_pts

  area_cm2 <- pi * 7 ^ 2

  pointwise_vol_above <- avg_pos_dist_cm * area_cm2

  pointwise_vol_below <- abs( avg_neg_dist_cm * area_cm2 )

  pointwise_vol_total <- sum(pointwise_vol_above, pointwise_vol_below)

  return_list <- mget(x = c('pointwise_vol_above',
                            'pointwise_vol_below',
                            'pointwise_vol_total'),
                      envir = rlang::current_env(),
                      inherits = F)

  return(return_list)




}



# face-weighted approach ------------------------------------------------------

#' \lifecycle{experimental}
#' @title Compute volume departures from reference plane
#'
#' @description Computes area of each face and average distance of its 3
#'   vertices. Multiplies these values to yield a weighted "approximate volume"
#'   for each face. Sums the values based on whether the mean distance is above
#'   or below the reference # datum.
#'
#' @param mesh Object of class "mesh3d"
#' @param radius Radius in cm that the mesh was trimmed, see
#'   [`pre_process_mesh()`] and [`remove_cyl()`]
#' @return List of length 3; each element is a numeric vector of length 1
#'   containing the computed volume above or below the reference datum, or the
#'   sum of these two (absolute value, i.e. total displacement)
#' @export
#'
#' @example inst/examples/vol_faceweighted_example.R
#'
vol_faceweighted <- function(mesh, radius = 7) {

 # Initially I was trying to use vcgClost() but I think the
  # better approach is to use Morpho. I believe this function actually
  # calls the vcgClost function but the output is easier to deal with
  # and the distances are more intuitive to think about (i.e. from which mesh
  # to which mesh)

  mdobj <- Morpho::meshDist(x = mesh,
                            mesh2 = ref_circ,
                            rampcolors = c("red", "white", "blue"),
                            plot = FALSE)


  # Create table containing vertex numbers and distances from mesh to target mesh

  vertex_nums_table <- tibble::tibble(vertex_number = seq_along(mdobj$dists),
                                      signed_distance = mdobj$dists)


  # create-vertex-lookup-table for each face

  faces <- suppressMessages(
    mdobj$colMesh$it %>%
      tibble::as_tibble(.name_repair = 'unique') %>%
      dplyr::mutate(vertex_type = paste0('vertex_', LETTERS[1:3])) %>%
      dplyr::relocate(.data$vertex_type, .before = dplyr::everything())
  )


  #####

  # Pivot table into long format to make both the face number and the vertex type individual variables

  faces_long <- faces %>%
    tidyr::pivot_longer(cols = dplyr::matches("\\d+"), #where(is.numeric), # apparently where() is not exported from any tidyverse packages
                        names_to = 'face_number',
                        values_to = 'vertex_number') %>%
    dplyr::mutate(face_number = as.integer(stringr::str_extract(.data$face_number, "\\d+"))) %>%
    dplyr::arrange(.data$face_number) %>%
    dplyr::relocate(.data$vertex_type, .after = .data$face_number)

  #####

  # Join faces_long table with the distances

  distances_table <- faces_long %>%
    dplyr::left_join(vertex_nums_table, by = 'vertex_number')

  #####

  # Take the average distance for each face

  avg_dists <- distances_table %>%
    dplyr::group_by(.data$face_number) %>%
    dplyr::summarise(mean_face_dist_mm = mean(.data$signed_distance))

  #####

  # Find the area for each face and join with `avg_dists` tibble

  mesh_areas <- Rvcg::vcgArea(mesh = mesh, perface = TRUE)

  areas <- tibble::tibble(face_number = 1:ncol(mdobj$colMesh$it),
                          face_area_mm2 = mesh_areas$pertriangle,
                          total_area_mm2 = 0.5*mesh_areas$area)


  combined_data <-  areas %>%
    dplyr::left_join(avg_dists, by = 'face_number') %>%
    dplyr::mutate(per_face_vol_mm3 = .data$mean_face_dist_mm * .data$face_area_mm2)


  #####

  # Calculate volume as the cross-sectional area times the single weighted average distance
  # this yields units of mm3, so convert to cm3 by multiplying by 0.1*0.1*0.1

  faceweighted_vol_above <- (0.1^3) * sum(combined_data$per_face_vol_mm3[combined_data$per_face_vol_mm3 >= 0])

  faceweighted_vol_below <- (0.1^3) * abs(sum(combined_data$per_face_vol_mm3[combined_data$per_face_vol_mm3 < 0]))

  faceweighted_vol_total <- sum(faceweighted_vol_above, faceweighted_vol_below)

  return_list <- mget(x = c("faceweighted_vol_above",
                            "faceweighted_vol_below",
                            "faceweighted_vol_total"),
                      envir = rlang::current_env(),
                      inherits = F)

  return(return_list)

}

