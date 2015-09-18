#' Create a heatmap based on movement data
#' 
#' @importFrom magrittr %>%
#' @importFrom plyr round_any
#' @importFrom purrr map reduce
#' @importFrom dplyr left_join
#' @import ggplot2
#' 
#' @param l A list of data frames containing columns for x- and y-values
#' @param x name of the column containing x-values in the data frames in
#'   \code{l} (string)
#' @param y name of the column containing y-values in the data frames in
#'   \code{l} (string)
#' @param blocksize Scaling factor
#' @param margins numeric vector of length 4 containing the margins (xmin, ymin,
#'   xmax, ymax)
#' @param origin (optional) numeric vector of length 2 (x and y) of the position
#'   to be removed from the heatmap (e.g. starting position)
#' @param consider.time logical indicating whether to count one subject multiple
#'   times in one cell, depending on the time the subject was inside the cell.
#' @param time.transformation if \code{consider.time} is \code{TRUE}, then
#'   \code{time.transformation} specifies the function to be applied to the
#'   position * time frequencies (default is \code{sqrt}). Can also be
#'   \code{NULL}.
#' @param zero.to.na logical indicating whether to recode unvisited cells to NA
#'   (default is TRUE)
#' @param print logical indicating if output should be printed via \code{ggplot}
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
                  time = FALSE, time_transform = sqrt, plot = FALSE)
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
    map(bin_2d, x, y, bin_width, margins) %>%
    # Add each case's raster to the empty raster
    map(right_join, y = empty_raster(bin_width, margins), by = c("x", "y")) %>%
    # Sum up data of all cases
    reduce(collapse_bins)
  
  # Set non-visited cells to NA
  if (drop)
  {
    bins[which(bins$b == 0), c("f", "b")] <- NA
  }
  
  # Apply a transformation to the frequency of visited cells
  if (!is.null(time_transform))
  {
    bins$f_transform <- do.call(time_transform, list(bins$f))
  }
  
  # Return either a plot or data frame
  if (plot)
  {
    fill_argument <- if (!time)
    {
      "b"
    }
    else if (is.null(time_transform))
    {
      "f"
    }
    else
    {
      "f_transform"
    }
    
    ggplot(bins, aes_string("x", "y", fill = fill_argument)) +
      geom_raster() +
      scale_fill_gradientn(colours = scale_Blues, na.value = rgb(0, 0, 0, 0)) +
      coord_fixed()
  }
  else
  {
    bins
  }
}

#' @importFrom purrr at_depth map
movhm_diff <- function(lx, ly, x, y, bin_width,
                       difference = c("relative", "absolute"), margins = NULL,
                       time = TRUE, time_transform = sqrt, plot = FALSE)
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
  
  bins <- map(list(lx, ly), movhm, x, y, bin_width, margins,
              time_transform = time_transform)
  
  bins <- diff_bins(bins[[1]], bins[[2]], length(lx), length(ly), difference)
  
  # Return either a plot or data frame
  if (plot)
  {
    fill_argument <- if (!time)
    {
      "b"
    }
    else if (is.null(time_transform))
    {
      "f"
    }
    else
    {
      "f_transform"
    }
    
    ggplot(bins, aes_string("x", "y", fill = fill_argument)) +
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

#' Find the margins in a list of lists or data frames
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map reduce
find_margins <- function(l, x, y, bin_width)
{
  map(l, find_margins_, x, y, bin_width) %>%
    reduce(function(x, y) data.frame(xmin = min(x[[1]], y[[1]]),
                                     ymin = min(x[[2]], y[[2]]),
                                     xmax = max(x[[3]], y[[3]]),
                                     ymax = max(x[[4]], y[[4]]))) %>%
    unlist()
}

#' Find the margins in one list or data frame
#' 
#' @importFrom dplyr summarise
#' @importFrom plyr round_any
#' @importFrom purrr map map_dbl
find_margins_ <- function(data, x, y, bin_width)
{
  # Create formulas like ~min(x)
  dots <-
    paste0(c(rep("~min(", 2), rep("~max(", 2)), c(x, y, x, y), rep(")", 4)) %>%
    map(as.formula) %>%
    setNames(c("xmin", "ymin", "xmax", "ymax"))
  
  # Return margins as named vector
  map_dbl(summarise_(data, .dots = dots), round_any, bin_width)
}

#' @importFrom dplyr filter between group_by summarize mutate ungroup
#' @importFrom magrittr %<>%
#' @importFrom plyr round_any
bin_2d <- function(.data, x, y, bin_width, margins = NULL)
{
  if (!is.null(margins))
  {
    .data %<>% apply_margins(x, y, margins)
  }
  
  # Round data to bin width
  positions <- data.frame(x = round_any(.data[[x]], bin_width),
                          y = round_any(.data[[y]], bin_width))
  
  # Count the number of times the case was in each cell (f) and what cells
  # were visited (b)
  positions %>%
    group_by(x, y) %>%
    summarize(f = n()) %>%
    ungroup() %>%
    mutate(b = 1)
}

# Delete all values that are outside of margins
apply_margins <- function(data, x, y, margins)
{
  data[data[[x]] >= margins[1] & data[[x]] <= margins[3] &
         data[[y]] >= margins[2] & data[[y]] <= margins[4], ]
}

#' Create an empty raster from margins and bin width
#'
#' @importFrom plyr round_any
empty_raster <- function(bin_width, margins)
{
  expand.grid(
    list(
      x = round_any(seq(margins[1], margins[3], bin_width), bin_width),
      y = round_any(seq(margins[2], margins[4], bin_width), bin_width)
    )
  )
}

#' Collapse two bins by summarizing the frequencies
collapse_bins <- function(x, y)
{
  # Set NAs to 0
  x[is.na(x$b), c("f", "b")] <- 0
  y[is.na(y$b), c("f", "b")] <- 0
  
  # Add frequencies
  x$f <- x$f + y$f
  x$b <- x$b + y$b
  
  x
}

# Get the difference between two bins
diff_bins <- function(bins_x, bins_y, len_x, len_y, difference)
{
  out <- data.frame(x = bins_x$x, y = bins_x$y)
  
  if (difference == "relative")
  {
    # Calculate relative frequencies
    bins_x[, c("f", "f_transform", "b")] %<>% `/`(len_x)
    bins_y[, c("f", "f_transform", "b")] %<>% `/`(len_y)
  }
  
  for (s in c("f", "f_transform", "b"))
  {
    out[[s]] <-
      map2(bins_x[[s]], bins_y[[s]],
           function(x, y) ifelse(x == 0 & y == 0, NA, x - y)) %>%
      unlist()
  }
  
  out
}
