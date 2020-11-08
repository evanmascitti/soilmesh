## code to prepare `untrimmed_mesh1` dataset goes here

raw_mesh <- Rvcg::vcgImport('inst/extdata/mesh004_w_color_131kfaces.ply', readcolor = T)
rot_trans_matrix <- matrix(data= c(0.61, 0.78, 0.17, 0, 0.73, -0.46, -0.51, 0, -0.32, 0.43, -0.84, 0, 179.1, -185.9, 364.42, 1), nrow = 4)

untrimmed_mesh1 <- Morpho::applyTransform(x= raw_mesh, trafo= rot_trans_matrix)

usethis::use_data(untrimmed_mesh1, overwrite = TRUE)
