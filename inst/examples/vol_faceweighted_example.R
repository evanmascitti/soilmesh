library(magrittr)
library(soilmesh)
library(mesheR)

trimmed <- remove_cyl(untrimmed_mesh1) %>%
  adjust_xy() %>%
  adjust_z()


rgl::clear3d()
mesheR::wiremesh3d(trimmed)
add_origin_axes()
mesheR::wiremesh3d(ref_circ)

vols <- vol_faceweighted(trimmed, radius = 7)

