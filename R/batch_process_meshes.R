#' @title Re-orient and trim meshes, then re-write to new files
#'
#' @description This function automates a process that I do repeatedly after collecting 3D scans.
#'
#' @param input_directory path to a directory containing one or more `.ply` files
#' @param output_directory path to a directory where the new files will be written
#' @param rtmatrix character string corresponding to one of the 4x4 matrices from the elements of [`soilmesh::rtmatrices()`]
#' @param overwrite whether to over-write existing files in the target directory (if they have identical names)
#'
#' @return Writes files to disk and prints a message
#' @export
#'

batch_process_meshes <- function(input_directory,
                                 output_directory,
                                 rtmatrix = NULL,
                                 overwrite = FALSE){

  if(missing(rtmatrix)){
    stop("\nNo roto-translation matrix supplied. Please use a character vector
    corresponding to one of the names in the `soilmesh::rtmatrices` data object.")}

  if(!(rtmatrix %in% names(soilmesh::rtmatrices))) {
    stop(
      "\nRoto-translation matrix argument does not match any of the matrices stored in the `soilmesh::rtmatrices` data object. Did you mis-type the date?"
    )
  }

  rot_trans_matrix <- soilmesh::rtmatrices[[rtmatrix]]


  start_time <- Sys.time()

  # add trailing slashes to directories, if not already present

  raw_dir <- if(stringr::str_sub(string = input_directory, start = -1) == "/"){
    input_directory} else{
      paste0(input_directory, "/")
    }

  processed_dir <- if(stringr::str_sub(string = output_directory, start = -1) == "/"){
    output_directory} else{
      paste0(output_directory, "/")
    }


  # error message if processed mesh files already exist in directory

  check_for_processed_meshes <- list.files(path = processed_dir,
                                           pattern = "[.]ply$",
                                           full.names = T) %>%
    length()

  if(check_for_processed_meshes != 0 & !overwrite){
    stop("\n`.ply files are already present in target directory.
         \nDid you use the wrong directory? \n
         If you really want to remove the existing files, use `overwrite = TRUE` to force overwrite.")
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


  # read the mesh files and compile them into a tibble and
  # create new columns containing the processed meshes, still at their full resolution,
  # and the file paths to which the processed meshes should be written.
  # Name the approriate columns so they match the argument names of Rvcg::vcgPlyWrite,
  # then select only the columns needed for writing the new files

  message(crayon::cyan("Reading mesh files..."))

  meshes <- soilmesh::parse_mesh_filename(x = raw_file_paths) %>%
    dplyr::mutate(mesh_object = purrr::map(.x = .data$old_path,
                                           .f = Rvcg::vcgImport),
                  mesh = purrr::map(.x = .data$mesh_object,
                                    .f = soilmesh::pre_process_mesh, rot_tr_matr = rot_trans_matrix),
                  filename = output_file_paths,
                  binary = FALSE)


  # Write the list of meshes to their new file paths

  purrr::pwalk(.l = meshes, .f = Rvcg::vcgPlyWrite)

  # check whether the files were written successfully by reading one back into R

  write_check <- length(Rvcg::vcgImport(
    file = list.files(processed_dir, full.names = T, pattern = "[.]ply$")[1] )
  )

  end_time <- Sys.time()

  time_diff <- lubridate::as.period(round(end_time - start_time, 2))

  if(write_check != 0){
    message(crayon::green(paste0(length(meshes$filename),
                                 " `.ply files written to disk. Run time was ",
                                 time_diff)))
  } else{
    stop("\nFiles could not be written to disk. \n
         Check that input and output directories exist and contain one or more `.ply` files.")
  }

}

