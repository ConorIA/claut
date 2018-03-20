claut
================

## Functions from the University of Toronto Climate Lab

[![build
status](https://gitlab.com/ConorIA/claut/badges/master/build.svg)](https://gitlab.com/ConorIA/claut/commits/master)
[![Build
status](https://ci.appveyor.com/api/projects/status/kioqnwa6xqs1f4j5?svg=true)](https://ci.appveyor.com/project/ConorIA/claut)

This package, currently at a very early stage of development, will
eventually host many of the functions generated at the University of
Toronto Climate Lab. For now, I (Conor Anderson) am the sole maintainer
of, and contributor to, this package, however, I hope that eventually
all of the miscellaneous functions that are produced in the lab can find
their way into this package. Any function contained in this package is
documented. Type `?function_name` in R to access this documentation. I
consider this to be a “working package”; the functions contained herein
are likely to change and/or may be split away into other projects
without warning.

## Installation

This package can be installed via the `install_git()` function in the
`devtools` or `remotes` packages, or by running the following
convenience script.

``` r
source("https://gitlab.com/ConorIA/claut/raw/master/install_claut.R")
```

## Functions used in papers in review

The following functions were used in studies that are currently under
review. If you are a reviewer, this is probably what you are looking
for. Note that these functions may undergo some minor optimization or
code changes, but the results that they produce will always be the
    same.

  - [`deltaDTD()`](https://gitlab.com/ConorIA/claut/blob/master/R/deltaDTD.R):
    Calculate a number of measures of diurnal temperature
    variability.
  - [`missing_value_lab()`](https://gitlab.com/ConorIA/claut/blob/master/R/missing_value_lab.R):
    A function to artificially introduce missing values into monthly
    climate series, replace those missing values with linear
    interpolation and cubic splines, and then calculate the error in the
    calculated mean.

## Misc functions

There are some other helper functions in this package that are here in
hopes that they prove useful to someone someday. These
    are:

  - [`trim_data()`](https://gitlab.com/ConorIA/claut/blob/master/R/trimData.R):
    An easy function to trim a `data.frame` to given start and end years
  - functions for working with ASCII gridded data; these were originally
    written to parse NOAA’s [GHCN Merged gridded data
    set](https://www.ncdc.noaa.gov/temp-and-precip/ghcn-gridded-products/)
      - [`parse_ASCII_grid()`](https://gitlab.com/ConorIA/claut/blob/master/R/parse_ASCII_grid.R):
        Reads an ASCII file of gridded data into a 3D matrix. This is
        currently quite slow and a little noisy, but it
        works.
      - [`generate_wkt_csv()`](https://gitlab.com/ConorIA/claut/blob/master/R/generate_wkt_csv.R):
        This function takes a list of matrix names and generates a
        `.csv` file of WKT-formatted polygons for import into QGIS and
        other GIS software.

## Other resources

Some of the functions that *were* contained in the package have been
moved to my [Conjuntool project](https://gitlab.com/ConorIA/conjuntool).
For other resources from the climate lab and beyond, please see our
[CL@UT Resources List](https://gitlab.com/ConorIA/claut-resources).

## Contributing to the `claut` package

The members of the Climate Lab are, first and foremost, academics and
researchers, not coders. If you would like to contribute code
improvements or patches to any of the functions here, please feel free
to open a [merge
request](https://gitlab.com/ConorIA/claut/merge_requests). Please note
that code you contribute will be attributed to you, and will be released
under the terms of the [GNU
GPLv3](https://gitlab.com/ConorIA/claut/blob/master/LICENSE.md).
