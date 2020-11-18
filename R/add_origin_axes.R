#' Draw colored axis lines
#'
#' **rgl** graphics device does not support axis lines such as those seen in
#' _MeshLab_. This function makes it simple to verify where the origin (i.e.
#' 0,0,0) lies in the viewer.
#'
#' @param xmin,xmax minimum and maximum extents of the lines in x direction
#' @param ymin,ymax minimum and maximum extents of the lines in y direction
#' @param zmin,zmax minimum and maximum extents of the lines in z direction
#' @param xcol,ycol,zcol colors for each line; specify with a quoted color name
#'   or hex code
#' @param lwd width of lines (see [rgl::rgl.material()])
#'
#' @return No return value; function is called for its side-effect of rendering
#'   the axes in the existing viewer window.
#' @export
#'
add_origin_axes <- function(xmin = -80, xmax = 80,
                            ymin = -80, ymax= 80,
                            zmin = -80, zmax = 80,
                            xcol = "red", ycol= "darkgreen", zcol= "darkblue",
                            lwd= 5){

  # create a data frame having 3 columns corresponding to the 6 points needed to
  # draw lines in the x, y, and z planes.

  axes_pts <- base::data.frame(
    x= c(xmin, xmax, 0, 0, 0, 0),
    y= c(0, 0, ymin, ymax, 0, 0 ),
    z= c(0, 0, 0, 0, zmin, zmax)
  ) %>%
    grDevices::xyz.coords()

  rgl::segments3d(axes_pts, color= c(xcol, ycol, zcol), lwd= lwd)
}
