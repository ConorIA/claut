claut
================

Functions from the University of Toronto Climate Lab
----------------------------------------------------

This package, currently at a very early stage of development, will eventually host many of the functions generated at the University of Toronto Climate Lab. For now, I (Conor Anderson) am the sole maintainer of, and contributor to, this package, however, I hope that eventually all of the miscellaneous functions that are produced in the lab can find their way into this package. Any function contained in this package is documented. Type `?function_name` in R to access this documentation.

Functions used in papers in review
----------------------------------

The following functions were used in studies that are currently under review. If you are a reviewer, this is probably what you are looking for. Note that these functions may undergo some minor optimization or code changes, but the results that they produce will always be the same.

-   dataEliminator: This set of functions is used to artificually introduce missing values into monthly climate series. In its current state, there are three variations of the function. These are likely to be refactored.
    1.  [`dataEliminator()`](https://gitlab.com/ConorIA/claut/blob/master/R/dataEliminator.R): The base function that eliminates data for a single year-month of data
    2.  [`dataEliminatorMassive()`](https://gitlab.com/ConorIA/claut/blob/master/R/dataEliminatorMassive.R): A helper function that calls the base function multiple times, e.g. 1000 repetitions of the same test
    3.  [`dataEliminatorThorough()`](https://gitlab.com/ConorIA/claut/blob/master/R/dataEliminatorThorough.R): A modified version of the base function that performs consecutive elimination of all possible combinations of *k* consecutive missing values

Misc functions
--------------

There are some other helper functions in this package that are here in hopes that they prove useful to someone someday. These are:

-   [`trim_data()`](https://gitlab.com/ConorIA/claut/blob/master/R/trimData.R): An easy function to trim a `data.frame` to given start and end years

Other packages
--------------

Members of the lab also use other packages that are too big to fit into this package. These are:

### Package we maintain:

-   [senamhiR: A Collection of Functions to Obtain Peruvian Climate Data](https://gitlab.com/ConorIA/senamhiR/)
-   [canadaHCDx: Additional functions for the canadaHCD package](https://gitlab.com/ConorIA/canadaHCDx/)

### Packages we use:

-   [canadaHCD: Canadian Historical Climate Data](https://github.com/gavinsimpson/canadaHCD/) by [Gavin L. Simpson](https://github.com/gavinsimpson)

Contributing
------------

The members of the Climate Lab are, first and foremost, academics and researchers, not coders. If you would like to contribute code improvements or patches to any of the functions here, please feel free to open a [merge request](https://gitlab.com/ConorIA/claut/merge_requests). Please note that code you contribute will be attributed to you, and will be released under the terms of the [GNU GPLv3](https://gitlab.com/ConorIA/claut/blob/master/LICENSE.md).
