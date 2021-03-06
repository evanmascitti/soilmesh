% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre_process_mesh.R
\name{pre_process_mesh}
\alias{pre_process_mesh}
\title{Process a 3D soil mesh for analysis}
\usage{
pre_process_mesh(mesh, rot_tr_matr, ...)
}
\arguments{
\item{mesh}{a mesh3d object}

\item{rot_tr_matr}{a 4x4 roto-translation matrix. User can select a matrix
from the date of calibration (these matrices will eventually be stored in the
\code{rot_tr_matrices} data object inside \strong{soilmesh}\verb{; the method development matrix already exists as an object ([}method_development_rtmatrix()`]).
Alternatively, the matrix can be manually specified.}

\item{...}{other arguments passed to \code{\link[=remove_cyl]{remove_cyl()}}, should user wish to
adjust the radius of retained mesh vertices}
}
\value{
processed mesh3d object
}
\description{
Pipe together several other internal processing functions to
allow a single call to adjust the mesh coordinates to real-world values.
}
\details{
Order of operations is important. The steps are as follows:
\enumerate{
\item Sample is rigidly (i.e. no deformation or scaling) re-oriented using a
user-specified roto-translation matrix.
\item Sample is trimmed of any points more than 50 mm beneath x-y plane. This
removes points registered from the scanner turntable or the cylinder base to
ensure step (3) is performed accurately.
\item Sample is adjusted in the x and y directions based on its largest values
in these dimensions; this returns the true center of the specimen to the
horizontal origin (i.e. x=0 and y=0)
\item Remove the specimen holder and the outermost ring of soil (default radius
is 70 mm per \code{\link[=remove_cyl]{remove_cyl()}})
\item Sample the z-elevations and adjust sample accordingly; see \code{\link[=adjust_z]{adjust_z()}}
}
}
