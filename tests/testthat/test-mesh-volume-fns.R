test_that("volume functions return values, not errors", {

  library(purrr)
  meshpaths <- dir(here::here('tests', 'testthat', 'test-meshes'), pattern = "cleatmark.*\\.ply$",
                   full.names = T)

meshes <- map(meshpaths, Rvcg::vcgImport) %>%
    tibble::enframe(name = "mesh_number", value = "mesh_object")

vol_above_mesh1 <- suppressWarnings({vol_slice_above(meshes$mesh_object[[1]])})

vol_below_mesh1 <- suppressWarnings({vol_slice_below(meshes$mesh_object[[1]])})


  # functions should return positive numeric
  expect_gt(vol_above_mesh1$vol_diff_cm3, 0)
  expect_gt(vol_below_mesh1$vol_diff_cm3, 0)

  # check that meshes are also returned

  expect_identical(class(vol_above_mesh1$sliced_mesh), "mesh3d")
  expect_identical(class(vol_below_mesh1$sliced_mesh), "mesh3d")


  # Try running the function over all the meshes
  suppressWarnings({
    above_vols <-meshes$mesh_object %>%
    map(vol_slice_above) %>%
    map_dbl("vol_diff_cm3")
  })

  are_positive <- all(above_vols > 0)
  expect_true(are_positive)

  ###############################################################################


# run all 4 functions over the list of meshes


  faceweighted <- lapply(meshes$mesh_object, vol_faceweighted)

  gridded <- lapply(meshes$mesh_object, vol_gridded)

  pointwise <- lapply(meshes$mesh_object, vol_pointwise)

  remeshed <- lapply(meshes$mesh_object, vol_remeshed)

  all_methods <- mget(c("faceweighted", "gridded", "pointwise", "remeshed"))

# check that none of the results contain NA values

 expect_false(any(is.na( Reduce(unlist, all_methods) )))



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
