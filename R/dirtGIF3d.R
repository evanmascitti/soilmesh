#' Save a GIF image of a spinning soil surface
#'
#' Showing the 3D scans in motion is a useful way to visualize cleat indentations
#' and visualize the surface microtopography.
#'
#' @param mesh mesh3d object to record
#' @param output_file relative file path to save (quoted, ending in ".gif")
#' @param phi vertical observer angle, see \link[rgl]{rgl.viewpoint}
#' @param zoom zooming factor, larger number is smaller object. Defaults to 0.8.
#' @param n_snaps number of snapshots to concatenate
#' @param fps speed at which frames are displayed (frames per second)
#' @param bg_color quoted color name or hex code for the scene background
#'
#' @return no return value; file is saved to disk
#' @export
#'
dirtGIF3d <- function(mesh, output_file, phi= -35, zoom = 0.8,
                  n_snaps = 10, fps = 2, bg_color = "grey60") {

  if(!requireNamespace("circular")){
    stop("\nThis function requires the `circular` package, please install it.")
  }

  if(!requireNamespace("magick")){
    stop("\nThis function requires the `magick` package, please install it.")
  }

  # set the screen size, background, and zoom parameters
  suppressWarnings(rgl::par3d(rgl::r3dDefaults) )
  rgl::par3d(windowRect = c(1707, 243, 2364, 900)) # these correspond to my laptop screen
  rgl::bg3d("grey60")
  rgl::rgl.viewpoint(phi = phi, zoom = zoom)
  rgl::rgl.pop("lights")
  rgl::rgl.light(phi = 60, specular = grDevices::grey.colors(8) %>% .[2])

  # for i in angles, first clear the window, then rotate the original mesh
  # object by i radians, and render it

  for (i in seq_along(1:n_snaps)) {

    rgl::clear3d()
    rgl::rotate3d(obj= mesh,
                  matrix = rgl::rotationMatrix(angle = circular::rad(360*i/n_snaps),
                                          x =0, y=0, z=1) ) %>%
      rgl::shade3d()


    # later I hope to add an option to render the axes as well (need to set this
    # in arguments;
    # default should be FALSE)
    # if(axes == TRUE){
    #   add_origin_axes()
    # }

    # set the file name for this iteration of the loop

    file_name <- tempfile(pattern = paste0("dirtsnap",
                                           formatC(i, digits = 4, flag = "0")),
                          fileext = ".png")

    # take the snapshot

    rgl::rgl.snapshot(filename = file_name)


  }

  # close 3d window

  rgl::rgl.close()

  # read all the tempfiles into a vector of magick objects

  all_snapshots <- magick::image_read(path = list.files(path = tempdir(),
                                                        pattern = "dirtsnap",
                                                        full.names = T))

  # combine the vector of images into a gif and write it to disk

  magick::image_write_gif(image = all_snapshots,
                          path = paste0(output_file, "_GIF", ".gif"), delay = 1/fps)

  # delete the temporary files so the temporary can be used again

  file.remove(list.files(path = tempdir(), pattern = "dirtsnap", full.names = T))

}
