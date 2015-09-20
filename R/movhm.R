#' movhm
#' 
#' @docType package
#' @name movhm_package
#' @useDynLib movhm
#' @importFrom Rcpp sourceCpp
NULL

#' Create a heatmap based on movement data
#' 
#' @importFrom magrittr %>%
#' @importFrom plyr round_any
#' @importFrom purrr map reduce
#' @importFrom dplyr left_join
#' @import ggplot2
#' 
#' @param l A list of named lists or data frames containing x- and y-values
#' @param x character string indicating the name of the variable holding
#'   x-values in the lists or data frames in \code{l}.
#' @param x character string indicating the name of the variable holding
#'   y-values in the lists or data frames in \code{l}.
#' @param bin_width numeric indicating the size of the bins
#' @param margins numeric vector of length 4 indicating the margins (xmin, ymin,
#'   xmax, ymax)
#' @param time logical indicating whether to count one case multiple
#'   times in one cell, depending on the time the subject was inside the cell.
#' @param time_transform if \code{time} is \code{TRUE}, then
#'   \code{time_transform} specifies the function to be applied to the
#'   position * time frequencies (default is \code{sqrt}). Can also be
#'   \code{NULL}.
#' @param drop logical indicating whether to set empty bins to NA instead of 0
#'   (default: \code{TRUE}).
#' @param plot logical indicating if the heatmap should be plotted
#' @return A data frame with x, y and f (freqency) columns ready for plotting
#'   with ggplot2
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' 
#' data(movdat)
#' 
#' movhm(movdat, x = "cart_x", y = "cart_y", bin_width = 50,
#'       margins = c(-750, -2000, 100, 1550)) %>%        
#'   ggplot(aes(x, y, fill = f)) +
#'   geom_raster() +
#'   scale_fill_gradientn(colours = scale_Blues, na.value = rgb(0, 0, 0, 0)) +
#'   coord_fixed() +
#'   theme_movhm()
movhm <- function(l, x, y, bin_width, margins = NULL, drop = TRUE,
                  time = TRUE, time_transform = NULL, plot = FALSE)
{
  # Find margins if they were not passed, otherwise round passed margins
  if (is.null(margins))
  {
    margins <- find_margins(l, x, y, bin_width)
  }
  else
  {
    margins <- round_any(margins, bin_width)
  }
  
  # Compute 2d raster
  bins <-
    l %>%
    # Apply 2d binning to each case
    map(bin_2d, x, y, bin_width, margins, time) %>%
    # Add each case's raster to the empty raster
    #map(right_join, y = empty_raster(bin_width, margins), by = c("x", "y")) %>%
    # Sum up data of all cases
    reduce(collapse_bins)
  
  if (!is.null(time_transform))
  {
    # Apply a transformation to the frequency of visited cells
    bins$f_transform <- do.call(time_transform, list(bins$f))
  }
  
  # Set non-visited cells to NA
  if (drop)
  {
    bins[which(bins$f == 0), "f"] <- NA
  }
  
  # Return either a plot or data frame
  if (plot)
  {
    ggplot(bins, aes_string("x", "y", fill = "f")) +
      geom_raster() +
      scale_fill_gradientn(colours = scale_Blues, na.value = rgb(0, 0, 0, 0)) +
      coord_fixed()
  }
  else
  {
    bins
  }
}

#' @importFrom plyr round_any
#' @importFrom purrr flatten map
#' @export
movhm_diff <- function(lx, ly, x, y, bin_width,
                       difference = c("relative", "absolute"), margins = NULL,
                       time = TRUE, time_transform = NULL, plot = FALSE)
{
  difference <- match.arg(difference)
  
  # Find margins if they were not passed, otherwise round passed margins
  if (is.null(margins))
  {
    margins <- find_margins(flatten(list(lx, ly)), x, y, bin_width)
  }
  else
  {
    margins <- round_any(margins, bin_width)
  }
  
  bins_list <- map(list(lx, ly), movhm, x, y, bin_width, margins,
                   time_transform = time_transform)
  
  bins <- bins[[1]]
  bins$f <- diff_bins(bins[[1]], bins[[2]], length(lx), length(ly), difference)
  
  # Return either a plot or data frame
  if (plot)
  {
    ggplot(bins, aes_string("x", "y", fill = "f")) +
      geom_raster() +
      scale_fill_gradientn(colours = scale_Spectral,
                           na.value = rgb(0, 0, 0, 0)) +
      coord_fixed()
  }
  else
  {
    bins
  }
}

#' @importFrom dplyr between filter group_by right_join summarize ungroup rename_ select_
#' @importFrom magrittr %>%
bin_2d <- function(.data, x, y, bin_width, margins, time)
{
  bins <-
    .data %>%
    select_(.dots = c(x, y)) %>%
    rename_(.dots = setNames(c(x, y), c("x", "y"))) %>%
    filter(between(x, margins[1], margins[3]),
           between(y, margins[2], margins[4])) %>%
    round_bins(bin_width) %>%
    group_by(x, y) %>%
    summarize(f = n()) %>%
    ungroup() %>%
    right_join(y = empty_raster(bin_width, margins), by = c("x", "y"))
  
  if (!time)
  {
    bins[which(bins$f > 0), "f"] <- 1
  }
  
  bins
}

#' Create an empty raster from margins and bin width
empty_raster <- function(bin_width, margins)
{
  expand.grid(
    list(
      x = round_bins(seq(margins[1], margins[3], bin_width), bin_width),
      y = round_bins(seq(margins[2], margins[4], bin_width), bin_width)
    )
  )
}

#' Collapse two bins by summarizing the frequencies
collapse_bins <- function(x, y)
{
  # Set NAs to 0
  x[is.na(x$f), "f"] <- 0
  y[is.na(y$f), "f"] <- 0
  
  # Add frequencies
  x$f <- x$f + y$f
  
  x
}

#' @export
round_bins <- function(x, bin_width, ...) UseMethod("round_bins")

#' @export
round_bins.default <- function(x, bin_width, ...)
{
  round(x / bin_width) * bin_width
}

#' @export
round_bins.data.frame <- function(data, bin_width, ...)
{
  data[] <- lapply(data, round_bins, bin_width, ...)
  
  data
}
