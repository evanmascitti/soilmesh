#' Shift a mesh up or down
#'
#' After using [adjust_xy()] to center the mesh, it must be moved up or down so
#' its origin lies in the x-y plane. `adjust_z()` computes the 10 most frequent
#' values in the mesh's z-coordinates and translates the mesh by this amount,
#' accounting for the direction of the "offset."
#'
#' @param mesh a "mesh3d" object
#' @param n_bins number of bins among which the coordinates should be split
#'
#' @return a "mesh3d" object with adjusted vertex coordinates
#' @importFrom rlang .data
#' @export
#'
#' @example inst/examples/adjust_z_example.R
adjust_z <- function(mesh, n_bins= 10000){

  # define a function for extracting the bin distances from the character string
  # which is returned by base::split()

  get_bounds <- function(x) {
    stringr::str_remove(string =  x, pattern = "\\(") %>%
      stringr::str_remove("]") %>%
      stringr::str_split(",")
  }

  # get vector of all z coordinates
  z_coords <- mesh$vb[3, ]

  # steps in calculating the z-error:
    # 1. split this vector into bins; defaults to 1000 but is over-ridden if a
    # value is supplied by user. This action returns a list.
    # 2. Make the list into a tibble
    # 3. Calculate the number of occurrences in each bin
    # 4. Create some new columns which extract the numeric values from the bins,
    # which R defines with a character string.
    # 5. Take the average of the two distance boundaries for that bin as the
    # distance which represents that bin's distance.
    # 6. Create a weighted "contribution" to the distance by multiplying the
    # number of occurrences in each bin by the representative distance for that
    # bin
    # 7. Calculate the weighted average of the top 10 most frequently occurring
    # distances and call it the z_error.


  z_top10_freq <- z_coords %>%
    base::split(base::cut(., breaks= n_bins)) %>%
    tibble::enframe() %>%
    dplyr::mutate(count= purrr::map_dbl(.data$value, ~base::length(.)),
                  bounds= get_bounds(.data$name) ,
                  lower= base::as.double(purrr::map(.data$bounds, ~.[1])),
                  upper= base::as.double(purrr::map(.data$bounds, ~.[2])) ,
                  avg=  (.data$lower + .data$upper)/2,
                  total_dist_per_bin= .data$count*.data$avg ) %>%
    dplyr::arrange(dplyr::desc(.data$count) ) %>%
    utils::head(n=10)

  z_error <- base::sum(z_top10_freq$total_dist_per_bin) / base::sum(z_top10_freq$count)

  # create a new vector which contains the correct z coordinates
  corrected_z_coords <- z_coords - z_error

  # copy the original mesh
  corrected_mesh <- mesh

  # assign the corrected z coordinates to the copied mesh
  corrected_mesh$vb[3, ] <- corrected_z_coords

  if(z_error > 5){
    rlang::warn(message = "The computed z-error was greater than 5 mm. Did you use `remove_cyl()` to clip the sample holder before computing the z-correction?\nIf the specimen has shrunk substantially, its surface may be somewhat concave but there is little choice but to ignore this message and assume the change in volume is equal for the portions above and below the planar surface.")
  }

  return(corrected_mesh)
}


