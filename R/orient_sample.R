#' Orient a 3D mesh in real-world coordinates
#'
#' The EinScan SP records points in a coordinate system where the camera eye is
#' (0,0,0). This is the first step in the pipeline, prior to other cleaning
#' operations (these are threefold: removing the sample holder; adjusting the x
#' and y coordinates so the specimen is precisely centered; moving the mesh up
#' or down so its original/undisturbed surface lies in the x-y plane)
#'
#' `orient_sample()` takes a "mesh3d" object and a 4x4 matrix as inputs and
#' passes them to [Morpho::applyTransform]. The matrix can be specified three
#' ways, in order of increasing preference:
#'
#' 1. Saved as an .rda file so can be accessed any time **soilmesh** is loaded
#' 2. Converted from a .csv or other flat file, then converted to a matrix with
#' `as.matrix()` 3. Manually inputted with `matrix()`
#'
#' Every time the scanner is moved or re-calibrated, a new matrix must be
#' computed. This function is designed to make that process less of a pain and
#' to ensure the entire pipeline is not broken irreversibly by accidentally
#' bumping the scanner.
#'
#' @param mesh a "mesh3d" object
#' @param rot_tr_matr a 4x4 matrix
#'
#' @return the rotated and translated "mesh3d" object
#' @export
#'
#' @example inst/examples/orient_sample_example.R
orient_sample <- function(mesh, rot_tr_matr){

  if(is.null(rot_tr_matr)){
    rlang::abort(message = "\n\nYou forgot to supply a roto-translation matrix. Please specify a matrix already installed in the soilmesh package, or provide a matrix manually.")
  }

  Morpho::applyTransform(x = mesh, trafo = rot_tr_matr)

}

