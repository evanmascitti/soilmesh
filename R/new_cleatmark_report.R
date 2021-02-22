#' @title Create a new report based on a template
#'
#' @description Report contains plots, a data summary, and images of the 3D mesh files.
#' By default, report YAML is populated to support both HTML and PDF output.
#'
#' @param file output path for report .Rmd file
#' @param ... other arguments passed to [`rmarkdown::draft()`]
#'
#' @return opens a new file in the RStudio IDE
#' @export
#'
new_cleatmark_report <- function(file, ...){

  rmarkdown::draft(file = file,
                   template = "cleatmark-device-daily-report",
                   package = "soilmesh",
                   edit = TRUE)
}
