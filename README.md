
<!-- README.md is generated from README.Rmd. Please edit that file -->

# soilmesh

<!-- badges: start -->

![packageversion](https://img.shields.io/badge/Package%20version-0.0.0.9007-orange.svg?style=flat-square)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--02--23-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

This package houses the functions I use for processing 3D meshes in ASI
468.

Rather than using point-and-click software (such as
[MeshLab](https://www.meshlab.net/)), this approach is buit on top of
other R packages and is done entirely with code. This ensures complete
reproducibility of my analyses and eliminates possible human errors,
which are hard to detect and trace.

This package has been kept separate from the functions in
**soiltestr** because the mesh processing methods are very specific to
my project, and it is not likely anyone else would need to use the
functions residing in **soilmesh**. I do plan to publish **soiltestr**
on CRAN (with a better name), so I want to be a good curator of that
package and keep my niche stuff out of it.
