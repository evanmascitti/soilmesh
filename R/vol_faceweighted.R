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
#'
#' @export
#'
#' @example inst/examples/vol_faceweighted_example.R
#'
vol_faceweighted <- numerical_area <- function(mesh, radius = 7) {

# First calculate distance from mesh to the refcirc object....This

#####



# Initially I was trying to use vcgClost() but I think the
  # better approach is to use Morpho. I believe this function actually
  # calls the vcgClost function but the output is easier to deal with
  # and the distances are more intuitive to think about (i.e. from which mesh
  # to which mesh)

mdobj <- Morpho::meshDist(x = mesh,
                          mesh2 = soilmesh::ref_circ,
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

above_vol_faceweighted_cm3 <- (0.1^3) * sum(combined_data$per_face_vol_mm3[combined_data$per_face_vol_mm3 >= 0])

below_vol_faceweighted_cm3 <- (0.1^3) * abs(sum(combined_data$per_face_vol_mm3[combined_data$per_face_vol_mm3 < 0]))

total_vol_faceweighted_cm3 <- sum(above_vol_faceweighted_cm3, below_vol_faceweighted_cm3)

vols_faceweighted_cm3 <- list(above_vol_faceweighted_cm3 = above_vol_faceweighted_cm3,
                          below_vol_faceweighted_cm3 = below_vol_faceweighted_cm3,
                          total_vol_faceweighted_cm3 = total_vol_faceweighted_cm3)

return(vols_faceweighted_cm3)

}
