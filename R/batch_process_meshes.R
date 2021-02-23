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
#'   be reduced. Defaults to 100k. Over-ride to a percent by supplying a numeric
#'   value with `percent = <0-1>`, which is passed to  [Rvcg::vcgQEdecim()]
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
                                 n_faces = 100000,
                                 overwrite = FALSE,
                                 ...
                                 ){


# assign directories and paths as character vectors ----------------------------------

  if(!requireNamespace("here")){
    stop("\nThis function requires the `here` package; please install it.")
  }

  if(missing(date)){
    stop("No date supplied. Date is required to search for mesh files.")
  }

  # use date argument to construct path to directory containing input files

  raw_dir <- paste0(here::here("analysis/data/raw_data/raw_meshes"),
                   "/", date, "_raw_meshes/")

  # create a new directory to hold the processed mesh files

  processed_dir <- paste0(here::here("analysis/data/derived_data/processed_meshes"), "/",
                          date, "_processed_meshes/")

  # throw an error if the directory already exists
  if(dir.exists(processed_dir) && overwrite == FALSE){
    stop("\n
         There is already a directory at the output path.
         Halting function call to prevent-over-write.")
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
  # user input. If a character string, look for the name of the matrix in the
  # soilmesh::rtmatrices object. If a matrix, take input as is.

  if(is.character(rtmatrix)){
    rot_trans_matrix <- soilmesh::rtmatrices[[rtmatrix]]
  } else{
    rot_trans_matrix <- rtmatrix
  }

  # error message if processed mesh file names already exist in output directory

  check_for_processed_meshes <- length(
    list.files(path = processed_dir,
               pattern = "[.]ply$",
               full.names = T)
  )

  if(check_for_processed_meshes != 0 && !overwrite){
    stop("\n`.ply files are already present in target directory.
         \nDid you use the wrong directory? \n
         If you really want to remove the existing files, use `overwrite = TRUE` to force overwrite.")
  }



  # start stopwatch
  start_time <- Sys.time()


# read the mesh files, compile them into a tibble and
  # create new columns containing the processed meshes (still at their full resolution).
  # Add a new column containing the file paths to which the processed meshes should be written.
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
                                      .f = Rvcg::vcgQEdecim, tarface = n_faces, ...))
  }

  # Write the list of meshes to their new file paths

  # Create the directory first....this step was not performed at top so
  # that if the function fails for another reason earlier in the call,
  # this directory is not written.

  dir.create(processed_dir)
  purrr::pwalk(.l = meshes, .f = Rvcg::vcgPlyWrite)



# check whether the files were written successfully -----------------------


  # count number of .ply files now existing in output directory

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
