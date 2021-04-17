test_that("file structure is correct", {

  # Set up the pre-requisites for the expectations --------------------------

  library(magrittr)

  # delete all the relevant files if they already exist

  unlink(x = here::here("ecmdata/raw-data/cleat-mark-testing/2021-04-17"),
         recursive = T, force = T)

# create parent directories to simulate one of my projects if they don't
  # already exist (I might just decide to leave them there permanently for other
  # interactive testing)

  parent_dirs <- c('ecmdata', 'ecmdata/raw-data', 'ecmdata/raw-data/cleat-mark-testing')

  purrr::walk(parent_dirs[!dir.exists(parent_dirs)], dir.create)

# run the function to create the folders and files

  cleat_mark_datasheets(
    experiment_name = 'my-experiment',
    sample_names = paste0('mix', 1:4),
    date = '2021-04-17',
    tin_tare_set = '2021-03-04',
    bowl_tare_set = '2021-04-10',
    sand_loose_density = 1.54,
    mini_density_reps = 1,
    drydown_tin_numbers = 1:24,
    mini_density_tin_numbers = 25:36
    )


# check if the files were created correctly

 expect_identical(
    file.exists(c(
     here::here("ecmdata/raw-data/cleat-mark-testing/2021-04-17/color-photos", "color-photos-index_2021-04-17.csv"),
      here::here("ecmdata/raw-data/cleat-mark-testing/2021-04-17/other-data", "cleat-mark-backfill-data_2021-04-17.csv"),
      here::here("ecmdata/raw-data/cleat-mark-testing/2021-04-17/other-data", "drydown-data_2021-04-17.csv"),
      here::here("ecmdata/raw-data/cleat-mark-testing/2021-04-17/other-data", "mini-density-data_2021-04-17.csv"),
      here::here("ecmdata/raw-data/cleat-mark-testing/2021-04-17/other-data", "penetrometer-data_2021-04-17.csv")
    )),
    rep(TRUE, 5)
    )

})

