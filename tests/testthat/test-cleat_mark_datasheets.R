# create parent directories to simulate one of my projects

parent_dirs <- c('ecmdata', 'ecmdata/raw-data', 'ecmdata/raw-data/cleat-mark-testing')

purrr::walk(parent_dirs, dir.create)


###############################################################################

# run tests


###############################################################################



test_that("directory structure is correct", {

  dirs <- paste('ecmdata/raw-data/cleat-mark-testing',
            c('raw-meshes',
            'color-photos',
            'other-data'),
            sep = "/")

  cleat_mark_datasheets(experiment_name = "my-experiment",
                        date = "2021-04-17",
                        sample_names = as.character(1:4),
                        tin_tare_set = "2021-03-04",
                        bowl_tare_set = "2021-04-10",
                        sand_loose_density = 1.54,
                        mini_density_reps = 1
                        )

  testthat::expect_idenical(dir.exists(paths = dirs),
                            list.files('ecmdata/raw-data/cleat-mark-testing'))

})

###############################################################################

test_that("all cleat mark data files exist", {

  data_files <- paste(
    here::here("ecmdata/raw-data/cleat-mark-testing"),
    c(
  "other-data/cleat-mark-backfill-data_2021-04-17.csv",
  "other-data/drydown-data_2021-04-17.csv",
  "other-data/mini-density-data_2021-04-17.csv",
  "other-data/penetrometer-data_2021-04-17.csv",
  "color-photos/color-photos-index_2021-04-17.csv"
  ),
  sep = "/")


  expect_identical(
    data_files,
    list.files(path = "./ecmdata/raw-data/cleat-mark-testing",
               recursive = T,
               full.names = T))

  })


###############################################################################

# remove the directories recursively so R CMD CHECK won't complain,
# and so the test can be re-run next time

unlink(parent_dirs, recursive = TRUE)
