
<!-- README.md is generated from README.Rmd. Please edit that file -->

# soilmesh

<!-- badges: start -->

![packageversion](https://img.shields.io/badge/Package%20version-0.0.0.9000-orange.svg?style=flat-square)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--11--07-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

This package houses the functions I use for processing 3D meshes in ASI
468. As of 2020-11-07 it holds a single function for clipping meshes to
just the soil surface.

I plan to add additional functions for auto-aligning meshes and
analyzing their geometry. THe goal is for there to be zero
point-and-click in the pipeline; this ensures complete reproducibility
of my analyses.

This package has been kept separate from that in **diRtscience** because
the mesh processing methods are very specific to my project, and it is
not likely anyone else would need to use the functions residing in
**soilmesh**. I do plan to publish **diRtscience** on CRAN, so I want to
be a good curator of that package and keep my niche stuff out of it.
