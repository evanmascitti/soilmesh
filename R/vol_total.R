#'\lifecycle{experimental}
#'
#' @title Compute total volume of disturbed soil after a cleat-mark test
#'
#' @description Makes internal calls to [`vol_below()`] and [`vol_above()`] and
#'   sums the results.
#'
#' @param mesh triangular mesh of class "mesh3d"
#'
#' @return volume of soil disturbed during the test in cm3
#' @export
#'
vol_total <- function(mesh){

  above_vol <- vol_above(mesh)
  below_vol <- vol_below(mesh)

  total_vol <- above_vol + below_vol

  return(total_vol)
}
