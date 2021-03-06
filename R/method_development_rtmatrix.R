#' method_development_rtmatrix
#'
#' Rigid translation and rotation used during 2020 development of soil mesh
#' processing pipeline
#'
#' @format A 4x4 matrix
#' \describe{ This is the matrix I created empirically by
#'   scanning an empty soil specimen holder with a round steel plate on top. I
#'   then used MeshLab to align the mesh to the x-y plane using the \emph{Rotate
#'   to fit a plane} filter and manually tweaked it a bit more with the
#'   Alignment tool. I then recorded the values visible in the lower-right
#'   corner of the screen and saved them as a .csv file. That file has been
#'   converted to the R data object which is manifested here. Updated
#'   2020-11-17.}
#'
"method_development_rtmatrix"
