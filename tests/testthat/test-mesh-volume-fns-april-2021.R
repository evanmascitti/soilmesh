test_that("volume functions return values, not errors", {

  library(purrr)
  meshpaths <- dir(here::here('tests', 'testthat', 'test-meshes'), pattern = "cleatmark.*\\.ply$",
                   full.names = T)

# print(meshpaths)

 meshes <- map(meshpaths, Rvcg::vcgImport) %>%
    tibble::enframe(name = "mesh_number", value = "mesh_object")

  vola1 <- vol_above(meshes[1, ]$mesh_object[[1]])
  volb1 <- vol_below(meshes[1, ]$mesh_object[[1]])


  # functions should return positive numeric
  expect_gt(vola1$vol_diff_cm3, 0)
  expect_gt(volb1$vol_diff_cm3, 0)

  # check that meshes are also returned

  expect_identical(class(vola1$sliced_mesh), "mesh3d")
  expect_identical(class(volb1$sliced_mesh), "mesh3d")


  # Try running the function over all the meshes
  above_vols <-meshes$mesh_object %>%
    map(vol_above) %>%
    map_dbl("vol_diff_cm3")

  are_positive <- all(above_vols > 0)
  expect_true(are_positive)

  ###############################################################################

  # some extra stuff for interactive testing

  # library(Morpho)
  # library(rgl)

  # volrefcirc <- vol_above(translate3d(ref_circ, x = 0, y= 0, z= 0.1))

  # clear3d()
  # shade3d(mesh1, color = inf_col)
  # shade3d(translate3d(ref_circ, x = 0, y= 0, z= 0.1))
  # clear3d()
  # wire3d(vola1$sliced_mesh, color = 'grey75')
  # plotNormals(vola1$sliced_mesh, length = 0.5)

  # clear3d()
  # shade3d(volb1$sliced_mesh, color = 'grey75')
  # plotNormals(volb1$sliced_mesh, length = 0.5)

  # list(vola1, volb1) %>%
    # purrr::map_dbl("vol_diff_cm3")
  # )

})
