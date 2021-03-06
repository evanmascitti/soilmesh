% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mesh-volume-fns.R
\name{vol_pointwise}
\alias{vol_pointwise}
\title{\lifecycle{experimental}
Compute volume from distances of every point on the mesh}
\usage{
vol_pointwise(mesh)
}
\arguments{
\item{mesh}{object of class "mesh3d"}
}
\value{
list of length 3 (above, below, total volumes in cm^3^)
}
\description{
Uses number of points above and below datum to compute a weighted
average of the soil volume above and below datum
}
