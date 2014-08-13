# movhm

A R package for displaying and analyzing group movement data from e.g. evacuation research or virtual reality studies.

## Installation

The development version can be installed using:

```R
# install.packages("devtools")
devtools::install_github("dgromer/movhm")
```
## Getting started

Input format are two-dimensional spatial point patterns from tracking devices or simulation output. If you have your data in text files, use `read.table` to load them (see `?read.table` for further information)

```R
# List of filenames
fn <- list("filename1.txt", "filename2.txt", "filename3.txt")

# Load all logfiles into list
logfiles <- lapply(fn, read.table, header = TRUE, sep = "\t")
```

If your files contain more columns than two columns for position data, use `subset` or `dplyr::select` to get rid of unneeded data

```R
library(dplyr)

# Select only x and y values (coord_x and coord_y are column names)
logfiles <- lapply(logfiles, select, coord_x, coord_y)
```

To print a heatmap use `movhm` (see `?movhm` for further information)

```R
movhm(logfiles, blocksize = .5, margins = c(-10, -10, 10, 10), print = TRUE)
```