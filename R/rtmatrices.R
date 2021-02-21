#'
#' @title List of roto-translation matrices manually calibrated with MeshLab
#'
#' @description This list object allows the scanner to be moved around and
#'   re-calibrated without losing the ability to easily process the meshes with
#'   a rigid rotation and translation to return the mesh to real-world
#'   coordinates.
#'
#' @format a list of data frames (each having the "tbl_df" class) with names
#'   corresponding to the date they were collected. The sole matrix from 2020 is
#'   the method development matrix and should not be used for later analyses.
#'
"rtmatrices"
