#' \lifecycle{experimental}
#'
#' @title Re-orient and trim meshes, then re-write to new files
#'
#' @description This function automates a process that I do repeatedly after
#'   collecting 3D scans.
#'
#' @param date character string corresponding to date 3D scans were collected
#' @param rtmatrix a 4x4 matrix. Normally supplied by passing a character string
#'   corresponding to one of the YYYY-MM-DD dates in the `soiltestr::rtmatrices`
#'   data object. Alternatively, pass a matrix either from the aforementioned
#'   data object or a matrix present as a variable in the existing global
#'   environment. [`soilmesh::rtmatrices()`]
#' @param downsample Whether to reduce the number of faces by calling
#'   [Rvcg::vcgQEdecim()]
#' @param n_faces if `downsample = TRUE`, number of faces to which mesh should
#'   be reduced. Defaults to 250k. Over-ride to a percent by supplying a numeric
#'   value with `percent = <0-1>`, which is passed to  [Rvcg::vcgQEdecim()]
#' @param recolor whether to re-color the meshes based on the average hex color of a digital image taken during testing
#' @param write_cols whether to write a csv file containing the hex color codes for each specimen (into the /derived_data/\<date\> directory)
#' @param save_metrics whether to write a csv file containing morphometrics data derived from this set of meshes
#' @param dne_tarface number of faces to further downsample mesh to compute DNE metric
#' @param snapshots whether to capture screenshots of the meshes (by default, both 16x9 and 4x3 are written)
#' @param overwrite whether to over-write existing files in the target directory
#'   (if they have identical names)
#' @param ... Other arguments passed to [Rvcg::vcgQEdecim()]
#'
#' @return Writes files to disk and prints a message.
#' @export
#'

batch_process_meshes <- function(date,
                                 rtmatrix = NULL,
                                 downsample = TRUE,
                                 n_faces = 250000,
                                 recolor= TRUE,
                                 write_cols = TRUE,
                                 save_metrics = TRUE,
                                 dne_tarface = 10000,
                                 snapshots = TRUE,
                                 overwrite = FALSE,
                                 ...
                                 ){

#browser()
# assign directories and paths as character vectors ----------------------------------

  if(!requireNamespace("here")){
    stop("\nThis function requires the `here` package; please install it.")
  }

  if(missing(date)){
    stop("No date supplied. Date is required to search for mesh files.")
  }

  # use date argument to construct path to directory containing input files

  raw_meshes_dir <- paste0(here::here("analysis/data/raw_data/cleat_mark_testing",
                               date, "raw_meshes"))


  #  build an experiment-level directory path for derived cleat mark data, but don't write it yet.
  # if this directory does not already exist, create it now.

  experiment_level_derived_cleatmark_data_dir <- paste0(
    here::here("analysis/data/derived_data/cleat_mark_testing")
    )

  if(!dir.exists(experiment_level_derived_cleatmark_data_dir)){
    dir.create(experiment_level_derived_cleatmark_data_dir)
  }

  #  build a directory path for ALL of today's cleat mark data, but don't write it yet
  # throw error if this directory already exists.

 daily_data_directory <- paste0(experiment_level_derived_cleatmark_data_dir,
                                "/",
                                date)

  if(dir.exists(daily_data_directory) && overwrite == FALSE){
    stop("\n
         There is already a directory at the output path.
         Halting function call to prevent-over-write.
         Use `overwrite = TRUE` to force over-write.")
  }

  # create a path for a new directory to hold the processed mesh files.
 # Don't actually write this sub-directory yet.

  processed_meshes_dir <- paste0(daily_data_directory, "/processed_meshes")


  # make a vector of file paths to read
  raw_mesh_file_paths <- list.files(path = raw_meshes_dir,
                               pattern = "ply$",
                               full.names = T)

  # make a vector of file paths to write
  output_mesh_file_paths <- paste0(processed_meshes_dir,
                              "/",
                              stringr::str_remove(string = basename(raw_mesh_file_paths),
                                                  pattern = "[.]ply$"),
                              "_processed.ply"
  )

  # some error messages if user has mis-specified any arguments

  if(missing(rtmatrix)){
    stop("\nNo roto-translation matrix supplied. Please use one of:
         \n1) A character vector corresponding to one of the dates in the `soilmesh::rtmatrices` data object
    or \n2) A 4x4 matrix")}

  if(is.character(rtmatrix) && !rtmatrix %in% names(soilmesh::rtmatrices)) {
    stop(
      "\nRoto-translation matrix argument does not match any of the matrices stored in the `soilmesh::rtmatrices` data object. Did you mis-type the date?"
    )
  }

  # conditionally assign the roto-translation matrix to a variable, depending on the
  # user input. If a character string, look for the name of the matrix in the
  # soilmesh::rtmatrices object. If a matrix, take input as is.

  if(is.character(rtmatrix)){
    rot_trans_matrix <- soilmesh::rtmatrices[[rtmatrix]]
  } else{
    rot_trans_matrix <- rtmatrix
  }

  # start stopwatch
  start_time <- Sys.time()


# read the mesh files, compile them into a tibble and
  # create new columns containing the processed meshes (still at their full resolution).
  # Add a new column containing the file paths to which the processed meshes should be written.
  # Name the appropriate columns so they match the argument names of Rvcg::vcgPlyWrite(),
  # then select only the columns needed for writing the new files

  message(crayon::cyan("Reading mesh files..."))

  meshes <- soilmesh::parse_mesh_filename(x = raw_mesh_file_paths) %>%
    dplyr::mutate(mesh_object = purrr::map(.x = .data$full_path,
                                           .f = Rvcg::vcgImport),
                  mesh = purrr::map(.x = .data$mesh_object,
                                    .f = soilmesh::pre_process_mesh,
                                    rot_tr_matr = rot_trans_matrix),
                  filename = output_mesh_file_paths,
                  binary = FALSE) %>%
    dplyr::select(-.data$mesh_object)

  # if meshes are to be downsampled, replace the mesh column in the meshes tibble
  # with a smaller version
  if(downsample == TRUE){

    message(crayon::cyan("\nRe-sampling meshes..."))

    meshes <- meshes %>%
      dplyr::mutate(mesh = purrr::map(.x = .data$mesh,
                                      .f = Rvcg::vcgQEdecim,
                                      tarface = n_faces, ...))
  }


  # write directory to hold today's processed meshes - this needs to happen
  # before the re-coloring step because the hex code colors file is written
  # inside this directory

  dir.create(daily_data_directory)


  # if meshes are to be recolored, read in the image files and extract their colors.
  # Then color the meshes with this vector.

  if(recolor){

    if(!requireNamespace("ecmfuns") || !requireNamespace("imager")){
      stop("Recoloring the meshes requires the following packages:
           `ecmfuns`
           `imager`
           \n Please install both (ecmfuns is available only on github, see https://www.github.com/evanmascitti/ecmfuns")
    }

    message(crayon::cyan("Re-coloring meshes..."))


    # create vector of image files to read

    raw_color_image_data_dir <- here::here("analysis/data/raw_data/cleat_mark_testing",
                                           date,
                                           "color_photos")

    raw_image_paths <- list.files(path = raw_color_image_data_dir,
                              pattern = "[.]png|[.]jpg|[.]jpeg",
                              ignore.case = T,
                              full.names = T)

    # read file which contains the order that the images were recorded and
    # arrange table according to order they were taken. Since R automatically
    # orders the image files based on their name, they
    # will be in the order they were taken. No need to keep track of the image
    # file numbers generated by my iPhone camera.

    color_photos_index_path <- paste0(raw_color_image_data_dir, "/", date, "_color_photos_index.csv")

    color_photos_index <- readr::read_csv(file = color_photos_index_path,
                    col_types = readr::cols(cylinder_ID = readr::col_character()),
                    na = "-") %>%
      dplyr::arrange(.data$test_order) %>%
      dplyr::mutate(image_path = raw_image_paths)


    # add new column to color_photos_index tibble containing the "cimg" object,
    # then extract the hex code and select only relevant columns

    hex_colors <- color_photos_index %>%
      dplyr::mutate(img = purrr::map(.x = raw_image_paths, .f = imager::load.image),
                    hex_color = purrr::map_chr(.x = .data$img, .f = ecmfuns::hex_extract)) %>%
      dplyr::select(.data$cylinder_ID, .data$hex_color)

  # join the existing meshes tibble with the hex colors tibble by the cylinder ID
  # then over-write the existing mesh column with the re-colored meshes

  meshes <- meshes %>%
    dplyr::left_join(hex_colors) %>%
    dplyr::mutate(mesh = purrr::map(
      .x = .data$mesh,
      .f = mesheR::colorMesh,
      col = .data$hex_color
    ))
  }

  if(write_cols){

    # construct outfile path

    hex_colors_path <- paste0(daily_data_directory, "/", date, "_hex_colors.csv")

    # construct tibble
    hex_colors_tibble <- meshes %>%
      dplyr::select(.data$experiment_ID, .data$soil_ID, .data$date, .data$cylinder_ID,
                    .data$hex_color)

    # check if file exists and stop if overwrite is not TRUE
    if(file.exists(hex_colors_path) && !overwrite){
      stop("\nHex colors file already exists. Halting function call to prevent over-writing.")}

    readr::write_csv(x = hex_colors_tibble, file = hex_colors_path)

    }

  # Write the list of meshes to their new file paths

  # Create a directory to hold the derived meshes. This step was not performed at top so
  # that if the function fails for another reason earlier in the call,
  # this directory is not written.

  dir.create(processed_meshes_dir)

  message(crayon::cyan("Writing new mesh files..."))

  purrr::pwalk(.l = meshes, .f = Rvcg::vcgPlyWrite)



# check whether the files were written successfully -----------------------


  # count number of .ply files now existing in output directory

  n_new_files <- length(list.files(path = processed_meshes_dir,
                                   pattern = "[.]ply$"))


  # make sure the files are readable by reading the first one back into R

  readability_check <- Rvcg::vcgImport(
    file = list.files(path = processed_meshes_dir, full.names = T, pattern = "[.]ply$")[1],
    readcolor = TRUE)


 if(length(readability_check) != 0){
    message(crayon::green(paste0(
      "Successfully wrote ",
      n_new_files,
      " `.ply files to disk. Rendering the first mesh for visual confirmation (see rgl viewer window)."
    ))
    )
  } else{
    stop("\nReadable files could not be written to disk. \n
         Check that both input and output directories exist and contain one or more `.ply` files.")
  }

  mesheR::wiremesh3d(x = readability_check, col = 'grey75')
  soilmesh::add_origin_axes()

  Sys.sleep(5)

  # write snapshots if user has set argument to true

  if(snapshots){

    message(crayon::cyan("Saving snapshots..."))


    # create sub-directory to hold images

    snapshot_dir <- paste0(daily_data_directory, "/mesh_snapshots")


    # check if directory already exists

    if(dir.exists(snapshot_dir) && !overwrite){
      stop("Snapshot directory already exists. Use `overwrite = TRUE` to force over-write. ")
    }

    suppressWarnings(dir.create(snapshot_dir))


    # construct tibble of arguments to pass to mesh-snapshot in a pwalk call

    snapshot_args <- meshes %>%
      dplyr::mutate(outfile = paste0(snapshot_dir, "/", .data$experiment_ID, "_", .data$soil_ID, "_", date, "_cyl", .data$cylinder_ID),
      asp1 = "4x3",
      asp2 = "16x9",
      wire = FALSE,
      axes = FALSE,
      overwrite = dplyr::if_else(condition = overwrite == TRUE,
                                 TRUE,
                                 FALSE)
    ) %>%
      tidyr::pivot_longer(cols = c(.data$asp1, .data$asp2),
                          names_to = "type",
                          values_to = "aspect_ratio") %>%
      dplyr::arrange(.data$aspect_ratio, .data$cylinder_ID) %>%
      dplyr::select(.data$mesh, .data$outfile, .data$wire, .data$axes, .data$aspect_ratio, .data$overwrite)


    # write the files

    purrr::pwalk(snapshot_args, soilmesh::mesh_snapshot)

    # this is a hack, but to fix the aspect ratio (cannot get correct with rgl, maybe still due to the viewport
    # read the 4x3 meshes back in and crop them, then re-write them to identical file paths, thus over-writing the originals

# dplyr::filter(snapshot_args, aspect_ratio == "4x3")
  }

# compute mesh metrics to save to disk ------------------------------------

if(save_metrics){

  # construct file path for output data

  mesh_metrics_path <- paste0(daily_data_directory, "/", date, "_mesh_metrics.csv")

  # print message
  message(crayon::cyan("Computing mesh metrics..."))


  # write a function to do extra down-sampling and cleaning
  # it takes a user-supplied argument to allow the downsampling to
  # be flexible. Pmpush (the author of molaR) suggests 10k faces, so that
  # is the default in the top-level function call.

  if(!requireNamespace("molaR")){
    stop("This function requires the `molaR` package. Please install it.")
  }
  get_dne <- function(bigmesh, dne_tarface){

    Rvcg::vcgQEdecim(bigmesh, tarface = dne_tarface) %>%
      Rvcg::vcgClean(sel = c(2,1,4), iterate = T, silent = F) %>%
      Morpho::updateNormals() %>%
      molaR::DNE() %>%
      .$Surface_DNE
  }

  # compute the volume and surface area metrics

  mesh_metrics <- meshes %>%
    dplyr::mutate(vol_above = purrr::map_dbl(.data$mesh, soilmesh::vol_above),
                  vol_below = purrr::map_dbl(.data$mesh, soilmesh::vol_below),
                  vol_total = .data$vol_above + .data$vol_below,
                  surf_area_cm2 = (0.1*0.1)*purrr::map_dbl(.data$mesh, Rvcg::vcgArea),
                  rfi = .data$surf_area_cm2 / (7*7*pi),
                  dne_safely = purrr::map(.data$mesh, .f = purrr::safely(~get_dne(bigmesh = ., dne_tarface = dne_tarface),
                                     otherwise = NA_real_,
                                     quiet = F)),
                 # OLD WAY dne = purrr::map_dbl(.data$dne_safely, purrr::pluck, 1)
                 # c.f Jenny Bryam: USING BUILT-IN INDEXING FUNCTIONALITY OF MAP (USE A NAME OR A POSITION):
                 dne = purrr::map_dbl(.data$dne_safely, 1)
    )


  # print warning if any dne measurements threw an error

  if(any(is.na(mesh_metrics$dne))){

     n_dne_errors <- sum(is.na(mesh_metrics$dne))

     dne_error_cyls <- dplyr::filter(mesh_metrics, is.na(.data$dne)) %>%
      .$cylinder_ID %>%
      stringr::str_c(collapse = ", ")

     if(!requireNamespace("glue")){
       stop("This function requires the `glue` package. Please install it.")
     }
     warning(glue::glue("There were {n_dne_errors} errors while computing the DNE values.
                       They errors occurred for cylinders {dne_error_cyls}.
                       You may want to inspect the quality of these meshes?")
    )
  }


  # write the metrics to disk

  mesh_metrics_to_write <- mesh_metrics %>%
    dplyr::select(
      .data$experiment_ID,
      .data$soil_ID,
      .data$date,
      .data$cylinder_ID,
      .data$vol_above,
      .data$vol_below,
      .data$vol_total,
      .data$surf_area_cm2,
      .data$rfi,
      .data$dne
    )

  readr::write_csv(x = mesh_metrics_to_write, file = mesh_metrics_path)
}

  # stop stopwatch and print message

  end_time <- Sys.time()

  time_diff <- lubridate::as.period(round((end_time - start_time), 2))

  message(crayon::green(paste0("Operation complete. Run time was ",
      time_diff, ". Please verify that the files were written correctly.")))

}
