% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_meshfiles.R
\name{read_meshfiles}
\alias{read_meshfiles}
\title{Read multiple 3D mesh files}
\usage{
read_meshfiles(x, colors = TRUE, ...)
}
\arguments{
\item{x}{character vector of paths pointing to .ply files}

\item{colors}{whether to read mesh colors (defaults to TRUE)}

\item{...}{additional arguments passed to \code{\link[Rvcg:vcgImport]{Rvcg::vcgImport()}}}
}
\description{
A wrapper to automate reading of \code{.ply} files into R. Specify a
character vector of file paths which are compiled into a nested tibble with
one row per file. The names of the files are parsed with
\code{\link[=parse_mesh_filename]{parse_mesh_filename()}} to include the minimum amount of information
required to uniquely identify the mesh. Other data such as water content
can be added via joins to other data frames.
)
}
\seealso{
\code{\link[Rvcg:vcgImport]{Rvcg::vcgImport()}}, \code{\link[=parse_mesh_filename]{parse_mesh_filename()}}
}
