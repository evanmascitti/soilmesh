#' Wrapper to make rgl lighting parameters less shiny
#'
#' @return alters existing rgl scene or opens a new one
#' @export
#'
better_lights <- function(){

# rgl::par3d(r3dDefaults)
rgl::rgl.pop('lights')
rgl::bg3d('white')

# turn lights back on
rgl::rgl.light(phi = 60,
							 specular = grDevices::grey.colors(8)[2])

# I think the below is redundant

# improved_scene <- rgl::scene3d()
# params <- rgl::par3d()

# output_list <- list(
# 	scene = improved_scene,
# 	params = params
# )

# save as internal data object


# then write a function that reads both those files and
# sets the relevant scene and parameters



}
