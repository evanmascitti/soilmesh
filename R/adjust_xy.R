#' Correct x and y offset for a soil specimen
#'
#' After re-orienting and moving the specimen with [orient_sample], the minute error in the horizontal plane needs to be corrected prior to removing the aluminum sample holder and the outermost portion of the soil surface. This function computes the difference between the maximum and minimum values observed in the x and y directions and corrects the vertex coordinates so the center is truly located at x=0 and y=0.
#'
#' @param mesh a "mesh3d" object which is already oriented with its surface lying parallel to the x-y plane
#'
#' @return a "mesh3d" object with its center lying in the normal to the x-y plane
#' @export
#'
#' @example inst/examples/adjust_xy_example.R
adjust_xy <- function(mesh){

  # version 2, using matrices and vectors only (less steps; probably faster, but more importantly there is less chance for something to get mixed up...)

  x_coords <- mesh$vb[1, ]
  y_coords <- mesh$vb[2, ]

 x_offset <- base::max(x_coords) - abs(base::min(x_coords))
 y_offset <- base::max(y_coords) - abs(base::min(y_coords))

 corrected_x_coords <- x_coords - x_offset
 corrected_y_coords <- y_coords - y_offset

  xy_adj_mesh <- mesh

  xy_adj_mesh$vb[1, ] <- corrected_x_coords
  xy_adj_mesh$vb[2, ] <- corrected_y_coords

  return(xy_adj_mesh)

  # version 1, using transposition and tibbles
  # vertices_tibble <-mesh$vb %>%
  #   base::t() %>%
  #   tibble::as_tibble()
  #
  # x_offset <- base::max(vertices_tibble$V1) - base::abs(base::min(vertices_tibble$V1))
  #
  # y_offset <- base::max(vertices_tibble$V2) - base::abs(min(vertices_tibble$V2))
  #
  # corrected_vertices <- vertices_tibble %>%
  #   dplyr::mutate(V1= V1- .data$x_offset,
  #          V2= V2- .data$y_offset
  #   ) %>%
  #   base::t() %>%
  #   base::as.matrix()
  #
  # xy_adj_mesh <- mesh
  # xy_adj_mesh$vb <- .data$corrected_vertices
  #
  # return(xy_adj_mesh)

}
