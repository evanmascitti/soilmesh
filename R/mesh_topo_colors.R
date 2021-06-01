#' Show the z-elevations of a mesh with color
#'
#' @param mesh mesh3d object
#' @param origin_axes plot the axes?
#' @param file optional, provide a file stem (no extension) if the plot and mesh are to be saved
#' @param ... other arguments passed to `Morpho::meshDist()`
#'
#' @return plots the colored mesh in an rgl window
#' @export
#' @importFrom rlang `%||%`
#'
mesh_topo_colors <- function(mesh, file = NULL,
                             palette = NULL,
                             origin_axes = FALSE, ...){

  better_lights()

	dirtpal <- palette %||% colorRampPalette(colors = c("darkblue", "#A57251", "red"))

	topo_colors <- dirtpal(30)

	if(origin_axes){add_origin_axes()}

	Morpho::meshDist(x = mesh, mesh2 = ref_circ, rampcolors = topo_colors, save = !is.null(file), file = file, ...)
	}
