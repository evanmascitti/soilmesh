# code to save user matrix from rgl viewpoint parameters:

library(rgl)
open3d()


# render a mesh

shade3d(meshes$mesh_object[[1]], color = inf_col)

# use mouse to re-size window
# and adjust angle and zoom

# set viewpoint to a matrix - this controls the angle and zoom
# parameters . There are three separate things to set: the zoom, the matrix, and the viewport size.
rgl.viewpoint()

report_userMatrix <- rgl::par3d()$userMatrix
report_zoom <- rgl::par3d()$zoom
report_viewport <- par3d()$viewport


# to check that they work, reset to defaults
par3d(r3dDefaults)
rgl.viewpoint()

# now return to old position with the 3 values

par3d(
  userMatrix = report_userMatrix,
  zoom = report_zoom,
  viewport = report_viewport
)

# YES IT FINALLY FUCKING WORKS

# save as csv files (see data-raw prep files)

# test that it works (remember screen might be zoomed in so it will look
# like it is getting cut off - but it isn't)

