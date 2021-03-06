#' Create a heatmap based on movement data
#' 
#' @importFrom plyr round_any
#' @import dplyr
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
#' movhm(movdat, x = "cart_x", y = "cart_y", blocksize = 50,
#'       margins = c(-750, -2000, 100, 1550), origin = c(-120, -1000)) %>%        
#'   ggplot(aes(x, y, fill = f)) +
#'   geom_raster() +
#'   scale_fill_gradientn(colours = scale_Blues, na.value = rgb(0, 0, 0, 0)) +
#'   coord_fixed() +
#'   theme_movhm()
movhm <- function(l, x, y, blocksize, margins, origin = NULL,
                  consider.time = FALSE, time.transformation = sqrt,
                  zero.to.na = TRUE, print = FALSE)
{
  # Create raster data frame for each subject
  raster <- lapply(l, count.pos, x = x, y = y, blocksize = blocksize,
                   margins = margins, consider.time = consider.time,
                   time.transformation = time.transformation)
  
  # Summarise all raster data frames
  raster <- Reduce(function(x, y){ x$f <- x$f + y$f; return(x) }, raster)
  
  # Set cells with zero visits to NA (to plot these cells transparent)
  if (zero.to.na)
  {
    raster[raster$f == 0, "f"] <- NA
  }
  
  # Set start position to NA or 0
  if (!is.null(origin))
  {
    if (blocksize < 1)
    {
      origin <- round_any(origin, blocksize)
    } else
    {
      origin <- round(origin / blocksize)
    }
    
    if (zero.to.na)
    {
      raster[raster$x == origin[1] & raster$y == origin[2], "f"] <- NA
    } else
    {
      raster[raster$x == origin[1] & raster$y == origin[2], "f"] <- 0
    }
  }
  
  if (print)
  {
    raster %>%
      ggplot(aes(x, y, fill = f)) +
      geom_raster() +
      scale_fill_gradientn(colours = scale_Blues, na.value = rgb(0, 0, 0, 0)) +
      coord_fixed() +
      theme_movhm()
  } else
  {
    raster
  }
}

#' Create a difference heatmap based on movement data
#' 
#' @import dplyr
#' @import ggplot2
#' 
#' @param lx a list of data frames containing columns for x- and y-values
#' @param ly a list of data frames containing columns for x- and y-values
#' @param x name of the column containing x-values in the data frames in
#'   \code{lx} and \code{ly} (string)
#' @param y name of the column containing y-values in the data frames in
#'   \code{lx} and \code{ly} (string)
#' @param difference a character string specifying whether to compute
#'   \code{"relative"} (default) or \code{"absolute"} differences.
#' @param blocksize scaling factor
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
#' @param print logical indicating if output should be printed via ggplot
#' @return A data frame with x, y and f (freqency) columns ready for plotting
#'   with ggplot2
#' @export
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' 
#' data(movdat)
#' 
#' movhm.diff(movdat[1:21], movdat[22:42], x = "cart_x", y = "cart_y",
#'            blocksize = 50,  margins = c(-750, -2000, 100, 1550),
#'            origin = c(-120, -1000)) %>%        
#'   ggplot(aes(x, y, fill = f)) +
#'   geom_raster() +
#'   scale_fill_gradientn(colours = scale_RdBu, na.value = rgb(0, 0, 0, 0),
#'                        limits = c(-.4, .4)) +
#'   coord_fixed() +
#'   theme_movhm()
movhm.diff <- function(lx, ly, x, y, difference = "relative", blocksize,
                       margins, origin = NULL, consider.time = FALSE,
                       time.transformation = sqrt, print = FALSE)
{
  raster.x <- movhm(lx, x = x, y = y, blocksize = blocksize, margins = margins,
                    origin = origin, consider.time = consider.time,
                    time.transformation = time.transformation,
                    zero.to.na = FALSE)
  raster.y <- movhm(ly, x = x, y = y, blocksize = blocksize, margins = margins,
                    origin = origin, consider.time = consider.time,
                    time.transformation = time.transformation,
                    zero.to.na = FALSE)
  
  if (difference == "relative")
  {
    f <- mapply(function(x, y) ifelse(x == 0 & y == 0, NA, x - y),
                raster.x$f / length(lx), raster.y$f / length(ly))
  } else if (difference == "absolute")
  {
    f <- mapply(function(x, y) ifelse(x == 0 & y == 0, NA, x - y), raster.x$f,
                raster.y$f)
  } else
  {
    stop("Invalid input for parameter difference.")
  }
  
  raster <- data.frame(x = raster.x$x, y = raster.x$y, f = f)
  
  if (print)
  {
    raster %>%
      ggplot(aes(x, y, fill = f)) +
      geom_raster() +
      scale_fill_gradientn(colours = scale_Spectral,
                           na.value = rgb(0, 0, 0, 0)) +
      coord_fixed() +
      theme_movhm()
  } else
  {
    raster
  }
}

#' Count occurrences in position raster
#' 
#' @importFrom plyr round_any
#' @import dplyr
#' 
#' @param data a data frame containing columns for x- and y-values
#' @param x name of the column containing x-values (string)
#' @param y name of the column containing y-values (string)
#' @param blocksize scaling factor
#' @param margins numeric vector of length 4 containing the margins (xmin, ymin,
#'   xmax, ymax)
#' @param consider.time logical indicating whether to count one subject multiple
#'   times in one cell
#' @param time.transformation if \code{consider.time} is \code{TRUE}, then
#'   \code{time.transformation} specifies the function to be applied to the
#'   position * time frequencies (default is \code{sqrt}). Can also be
#'   \code{NULL}.
#' @return A data frame with x, y and f (freqency) columns
count.pos <- function(.data, x, y, blocksize, margins, consider.time = FALSE,
                      time.transformation = sqrt)
{
  # Subset data
  tmp <- select_(.data, as.name(x), as.name(y))
  
  # Set column names
  names(tmp) <- c("x", "y")
  
  # Apply blocksize and round to margins and values
  if (blocksize < 1)
  {
    margins <- round_any(margins, blocksize)
    
    data <- data.frame(x = round_any(tmp$x, blocksize),
                       y = round_any(tmp$y, blocksize))
  } else
  {
    margins <- round(margins / blocksize)
    
    data <- round(tmp / blocksize)
  }
  
  # Delete values out of borders
  data <- data %>%
    filter(x > margins[1], x < margins[3], y > margins[2], y < margins[4])  
  
  if (consider.time)
  {
    # Count the number of times subject was in each cell
    data <- data %>%
      group_by(x, y) %>%
      summarize(f = n())
    
    # Apply specified time transformation (default is sqrt)
    if (!is.null(time.transformation))
    {
      data$f <- do.call(time.transformation, list(data$f))
    }
  } else
  {
    # Extract each cell visited
    data <- data %>%
      unique() %>%
      mutate(f = 1)
  }
  
  # Create an empty data frame for raster values
  if (blocksize < 1)
  {
    raster <- expand.grid(
      list(
        x = round_any(seq(margins[1], margins[3], blocksize), blocksize),
        y = round_any(seq(margins[2], margins[4], blocksize), blocksize)
      )
    )
  } else
  {
    raster <- expand.grid(list(x = seq(margins[1], margins[3]),
                               y = seq(margins[2], margins[4])))
  }
  
  # Add values to raster data frame
  raster <- left_join(raster, data, by = c("x", "y"))
  
  # Set NA to 0 (is needed to summarise multiple raster data frames)
  raster[is.na(raster$f), "f"] <- 0
  
  # Return raster data frame
  raster
}
