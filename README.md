# movhm

An R package for displaying and analyzing group movement data from e.g. evacuation research or virtual reality studies.

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

## Plot a heatmap

To print a heatmap use `movhm` (see `?movhm` for further information)

```R
# Load example data set
data(movdat)

# Print a heatmap
movhm(movdat, x = "cart_x", y = "cart_y", blocksize = 50,
      margins = c(-750, -2000, 100, 1550), origin = c(-120, -1000),
	  print = TRUE)
```