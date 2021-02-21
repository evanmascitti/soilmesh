#' @title Re-orient and trim meshes, then re-write to new files
#'
#' @description This function automates a process that I do repeatedly after
#'   collecting 3D scans.
#'
#' @param input_directory path to a directory containing one or more `.ply`
#'   files
#' @param output_directory path to a directory where the new files will be
#'   written
#' @param rtmatrix a 4x4 matrix. Normally supplied by passing a character string
#'   corresponding to one of the YYYY-MM-DD dates in the `soiltestr::rtmatrices`
#'   data object. Alternatively, pass a matrix either from the aforementioned
#'   data object or a matrix present as a variable in the existing global
#'   environment. [`soilmesh::rtmatrices()`]
#' @param ... Other arguments passed to [Rvcg::vcgQEdecim()] (usually either
#'   `tarface` for a set number of faces or `percent` for a percentage of
#'   original face number (numeric, 0-1). Ignored if `downsample = TRUE`
#' @param downsample Whether to reduce the number of faces by calling
#'   [Rvcg::vcgQEdecim()]
#' @param overwrite whether to over-write existing files in the target directory
#'   (if they have identical names)
#'
#' @return Writes files to disk and prints a message
#' @export
#'

batch_process_meshes <- function(input_directory,
                                 output_directory,
                                 rtmatrix = NULL,
                                 ... ,
                                 downsample = TRUE,
                                 overwrite = FALSE
                                 ){

  # add trailing slashes to directories, if not already present

  raw_dir <- if(stringr::str_sub(string = input_directory, start = -1) == "/"){
    input_directory} else{
      paste0(input_directory, "/")
    }

  processed_dir <- if(stringr::str_sub(string = output_directory, start = -1) == "/"){
    output_directory} else{
      paste0(output_directory, "/")
    }


  # make a vector of file paths to read
  raw_file_paths <- list.files(path = raw_dir,
                               pattern = "ply$",
                               full.names = T,
                               recursive = F)

  # make a vector of file paths to write

  output_file_paths <- paste0(processed_dir,
                              stringr::str_remove(string = basename(raw_file_paths),
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
  # user input

  if(is.character(rtmatrix)){
    rot_trans_matrix <- soilmesh::rtmatrices[[rtmatrix]]
  } else{
    rot_trans_matrix <- rtmatrix
  }

  # error message if processed mesh file names already exist in output directory

  check_for_processed_meshes <- list.files(path = processed_dir,
                                           pattern = "[.]ply$",
                                           full.names = T) %>%
    length()

  if(check_for_processed_meshes != 0 & !overwrite){
    stop("\n`.ply files are already present in target directory.
         \nDid you use the wrong directory? \n
         If you really want to remove the existing files, use `overwrite = TRUE` to force overwrite.")
  }



  # start stopwatch
  start_time <- Sys.time()


# read the mesh files, compile them into a tibble and
  # create new columns containing the processed meshes (still at their full resolution).
  # Add the file paths to which the processed meshes should be written.
  # Name the appropriate columns so they match the argument names of Rvcg::vcgPlyWrite(),
  # then select only the columns needed for writing the new files

  message(crayon::cyan("Reading mesh files..."))

  meshes <- soilmesh::parse_mesh_filename(x = raw_file_paths) %>%
    dplyr::mutate(mesh_object = purrr::map(.x = .data$old_path,
                                           .f = Rvcg::vcgImport),
                  mesh = purrr::map(.x = .data$mesh_object,
                                    .f = soilmesh::pre_process_mesh,
                                    rot_tr_matr = rot_trans_matrix),
                  filename = output_file_paths,
                  binary = FALSE)

  # if meshes are to be downsampled, replace the mesh column in the meshes tibble
  # with a smaller version
  if(downsample == TRUE){

    message(crayon::cyan("\nRe-sampling meshes..."))

    meshes <- meshes %>%
      dplyr::mutate(mesh = purrr::map(.x = .data$mesh,
                                      .f = Rvcg::vcgQEdecim, ...))
  }


  # Write the list of meshes to their new file paths

  purrr::pwalk(.l = meshes, .f = Rvcg::vcgPlyWrite)


  ### check whether the files were written successfully

  # count number of .ply files in output directory

  n_new_files <- length(list.files(path = processed_dir,
                                   pattern = "[.]ply$"))


  # make sure the files are readable by reading the first one back into R

  readability_check <- Rvcg::vcgImport(
    file = list.files(path = processed_dir, full.names = T, pattern = "[.]ply$")[1] )


  end_time <- Sys.time()

  time_diff <- lubridate::as.period(round(end_time - start_time, 2))

  if(length(readability_check) != 0){
    message(crayon::green(paste0(
      "Successfully wrote ",
      n_new_files,
      " `.ply files to disk. Run time was ",
      time_diff,
      ". Rendering the first mesh for visual confirmation (see rgl viewer window)."
    ))
    )
  } else{
    stop("\nReadable files could not be written to disk. \n
         Check that both input and output directories exist and contain one or more `.ply` files.")
  }

  mesheR::wiremesh3d(x = readability_check, col = 'grey75')
  soilmesh::add_origin_axes()

}

