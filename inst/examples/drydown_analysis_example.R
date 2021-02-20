# # create temporary csv file from built-in dataset
#
# f <- file.path(tempdir(), "example_drydown_file.csv")
# readr::write_csv(x = soilmesh::example_drydown_data, file = f)
#
# data <- readr::read_csv(f)
#
#
# # perform analysis
#
# eda <- drydown_analysis(path = f)
