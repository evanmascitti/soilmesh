% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mesh-volume-fns.R
\name{vol_slice}
\alias{vol_slice}
\title{Calculate soil volume above or below a reference plane}
\usage{
vol_slice(mesh, direction, z_displ_mm = 0.01, include_ref = FALSE)
}
\arguments{
\item{mesh}{triangular mesh of class "mesh3d"}

\item{direction}{one of "above" or "below"}

\item{z_displ_mm}{vertical offset to prevent removal of vertices near the origin}

\item{include_ref}{whether the returned mesh object should include the
reference circle plane? Default is \verb{FALSE which} returns only the points
retained after slicing.}
}
\value{
list containing a numeric vector of length 1 (cm3 of soil
residing above the reference plane) and the resulting mesh from the slicing operation
}
\description{
One half of the total volume computation.
}
\details{
\lifecycle{experimental}

This applies a sort of hack to slice the mesh and keep only points
residing above or below the x-y plane. The "hack" portion is translating the
actual reference plane away from the mesh by a very small distance. The volume
attributable to the offset is subtracted before completing the calculation.
The meshes used in this project can \emph{never} be "watertight" and it is my
hope that so long as the mesh is 2-manifold, the volume computation is
accurate. This function calls \code{\link[Rvcg:vcgClean]{Rvcg::vcgClean()}} to remove non-manifold
faces, then remove unreferenced vertices, and (finally) remove non-manifold
vertices before coherently orienting the faces. This approach seems to resolve the non-manifoldness of the meshes
induced by downsampling, cylinder removal, and top/bottom removal.
}
\seealso{
\code{\link[=vol_gridded]{vol_gridded()}}, \code{\link[=vol_faceweighted]{vol_faceweighted()}}, \code{\link[=vol_pointwise]{vol_pointwise()}}
}
