#' movhm
#' 
#' @docType package
#' @name movhm_package
#' @useDynLib movhm
#' @importFrom Rcpp sourceCpp
NULL

#' Two-dimensional binning for group movement data
#' 
#' @importFrom magrittr %>%
#' @importFrom purrr map reduce
#' @importFrom ggplot2 aes_string ggplot geom_raster scale_fill_gradientn
#'   coord_fixed
#' 
#' @param l A list of data frames or named lists containing x- and y-values
#' @param x character string indicating the name of the variable holding
#'   x-values in the data frames or lists in \code{l}.
#' @param y character string indicating the name of the variable holding
#'   y-values in the data frames or lists in \code{l}.
#' @param bin_width numeric indicating the size of the bins
#' @param margins numeric vector of length 4 indicating the margins (xmin, ymin,
#'   xmax, ymax)
#' @param time logical indicating whether to count one case multiple
#'   times in one cell, depending on the time the subject was inside the cell.
#' @param time_transform if \code{time} is \code{TRUE}, then
#'   \code{time_transform} specifies the function to be applied to the
#'   position * time frequencies, e.g. \code{sqrt}. 
#' @param drop logical indicating whether to set empty bins to NA instead of 0 
#'   (default: \code{TRUE}). This is useful when plotting, so empty bins can be
#'   plotted transparent.
#' @param plot logical indicating if the heatmap should be plotted
#' @return A data frame with x, y and f (freqency) columns
#' @export
#' @examples
#' data(movdat)
#' 
#' bins <- movhm(movdat, x = "cart_x", y = "cart_y", bin_width = 50,
#'               margins = c(-750, -2000, 100, 1550))
#'
#' library(ggplot2)
#' ggplot(bins, aes(x, y, fill = f)) +
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
    margins <- round_bins(margins, bin_width)
  }
  
  # Compute 2d raster
  bins <-
    l %>%
    # Apply 2d binning to each case
    map(bin_2d, x, y, bin_width, margins, time) %>%
    # Sum up data of all cases
    reduce(collapse_bins)
  
  if (!is.null(time_transform))
  {
    # Apply a transformation to the frequency of visited cells
    bins$f<- do.call(time_transform, list(bins$f))
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

#' movhm_diff
#' 
#' @inheritParams movhm
#' @param lx A list of data frames or named lists containing x- and y-values
#' @param ly A list of data frames or named lists containing x- and y-values
#' @param difference character string indicating whether absolute
#'   (\code{"absolute"}) or relative (\code{"relative"}) frequency differences
#'   should be calculated.
#' 
#' @importFrom ggplot2 aes_string ggplot geom_raster scale_fill_gradientn
#'   coord_fixed
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
    margins <- round_bins(margins, bin_width)
  }
  
  bins_list <- map(list(lx, ly), movhm, x, y, bin_width, margins,
                   time_transform = time_transform)
  
  bins <- bins_list[[1]]
  bins$f <- diff_bins(bins_list[[1]]$f, bins_list[[2]]$f, length(lx),
                      length(ly), difference)
  
  # Return either a plot or data frame
  if (plot)
  {
    limit <- max(abs(bins$f), na.rm = TRUE)
    
    ggplot(bins, aes_string("x", "y", fill = "f")) +
      geom_raster() +
      scale_fill_gradientn(limits = c(-limit, limit), colours = scale_Spectral,
                           na.value = rgb(0, 0, 0, 0)) +
      coord_fixed()
  }
  else
  {
    bins
  }
}

#' @importFrom dplyr between filter group_by right_join summarize ungroup
#'   rename_ select_
#' @importFrom magrittr %>%
bin_2d <- function(.data, x, y, bin_width, margins, time)
{
  bins <-
    .data %>%
    # Drop all columns except x and y values
    select_(.dots = c(x, y)) %>%
    # Rename columns to "x" and "y"
    rename_(.dots = setNames(c(x, y), c("x", "y"))) %>%
    # Drop values that are out of margins
    filter(between(x, margins[1], margins[3]),
           between(y, margins[2], margins[4])) %>%
    # Put all values into bins
    round_bins(bin_width) %>%
    # Calculate the frequencies of each bin using group_by and summarize
    group_by(x, y) %>%
    summarize(f = n()) %>%
    ungroup() %>%
    # Add the frequencies to the empty raster
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
