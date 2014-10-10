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
                  consider.time = FALSE, zero.to.na = TRUE, print = FALSE)
{
  # Create raster data frame for each subject
  raster <- lapply(l, count.pos, x = x, y = y, blocksize = blocksize,
                   margins = margins, consider.time = consider.time)
  
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
                       print = FALSE)
{
  raster.x <- movhm(lx, x = x, y = y, blocksize = blocksize, margins = margins,
                    origin = origin, consider.time = consider.time,
                    zero.to.na = FALSE)
  raster.y <- movhm(ly, x = x, y = y, blocksize = blocksize, margins = margins,
                    origin = origin, consider.time = consider.time,
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
#' @return A data frame with x, y and f (freqency) columns
count.pos <- function(.data, x, y, blocksize, margins, consider.time = FALSE)
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

#' A blank theme for heatmaps
#' 
#' @import ggplot2
#' 
#' @param base_size base font size
#' @param base_family base font family
#' @export
theme_movhm <- function (base_size = 12, base_family = "")
{
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
}

#' Display the grid for roi analysis
#' 
#' @import ggplot2
#' 
#' @param template a list of ggplot2-geoms which will be plotted in the
#'   background.
#' @param blocksize scaling factor
#' @param margins numeric vector of length 4 containing the margins (xmin, ymin,
#'   xmax, ymax)
#' @param roi a two-column data frame with x- and y-values specifying what cells
#'   should be included in the ROI.
#' @export
show.me.da.raster <- function(template, blocksize, margins, roi = NULL)
{
  # dirty workaround here: we put x.max, y.max and roi into the global
  # environment because that's where ggplot's aes looks for them
  .x.min.movhm <<- round(margins[1] / blocksize)
  .y.min.movhm <<- round(margins[2] / blocksize)
  .x.max.movhm <<- round(margins[3] / blocksize)
  .y.max.movhm <<- round(margins[4] / blocksize)
  .roi.movhm <<- roi
  
  p <- ggplot() +
    #template() +
    # Grid
    geom_vline(xintercept = seq(.x.min.movhm + .5, .x.max.movhm, 1),
               colour = "red", alpha = .25) +
    geom_hline(yintercept = seq(.y.min.movhm + .5, .y.max.movhm, 1),
               colour = "red", alpha = .25) +
    # Coloured grid
    geom_rect(aes(xmin = seq(.x.min.movhm + .5, .x.max.movhm - .5, 2),
                  ymin = .y.min.movhm,
                  xmax = seq(.x.min.movhm + 1.5, .x.max.movhm + .5, 2),
                  ymax = .y.max.movhm),
              fill = "red", alpha = .05) +
    geom_rect(aes(xmin = .x.min.movhm,
                  ymin = seq(.y.min.movhm + .5, .y.max.movhm - .5, 2),
                  xmax = .x.max.movhm,
                  ymax = seq(.y.min.movhm + 1.5, .y.max.movhm + .5, 2)),
              fill = "red", alpha = .05) +
    scale_x_continuous(breaks = seq(.x.min.movhm, .x.max.movhm, 1),
                       limits = c(.x.min.movhm, .x.max.movhm),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(.y.min.movhm, .y.max.movhm, 1),
                       limits = c(.y.min.movhm, .y.max.movhm),
                       expand = c(0, 0)) +
    coord_fixed() +
    theme_bw() +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
  
  if(!is.null(roi))
  {
    p <- p +
      geom_rect(aes(xmin = .roi.movhm[[1]] - .5, ymin = .roi.movhm[[2]] - .5,
                    xmax = .roi.movhm[[1]] + .5, ymax = .roi.movhm[[2]] + .5),
                fill = "blue", alpha = .5)
    
  }
  
  print(p)
}

#' Execute a ROI analysis on movement data
#' 
#' @importFrom plyr round_any
#' @import dplyr
#' 
#' @param ... lists of two-column data frames with x- and y-values
#' @param x name of the column containing x-values in the data frames in
#'   \code{...} (string)
#' @param y name of the column containing y-values in the data frames in
#'   \code{...} (string)
#' @param roi a two-column data frame with x- and y-values specifying what cells
#'   should be included in the ROI.
#' @param blocksize scaling factor
#' @param margins numeric vector of length 4 containing the margins (xmin, ymin,
#'   xmax, ymax)
#' @export
roi.analysis <- function(..., x, y, roi, blocksize, margins, consider.time)
{
  objects <- list(...)
  
  if (length(objects) == 0)
  {
    stop("You need to specify at least one list of data frames to ...")
  }
  
  # Round roi values due to R's problems with numeric values (see FAQ 7.31)
  roi$x <- round_any(roi$x, blocksize)
  roi$y <- round_any(roi$y, blocksize)
  
  lapply(objects, sapply, function(data) {
    # Compute raster data frame for Ss
    raster <- count.pos(data, x = x, y = y, blocksize = blocksize,
                        margins = margins, consider.time = consider.time)
    
    # Count number of cells which occur in both roi and raster w/ f == 1
    nrow(inner_join(roi, filter(raster, f == 1), by = c("x", "y")))
  })
}

#' Colour gradients
#' 
#' @description
#' These colour gradients are used in conjunction with ggplot2's
#' \code{scale_fill_gradientn()}. Available gradients are
#' 
#' \describe{
#'   \item{\code{scale_Blues}}{
#'   A colour gradient for heatmaps, from white to blue
#'   }
#'   \item{\code{scale_RdGrBu}}{
#'   A colour gradient for difference heatmaps, from red over green to blue
#'   }
#'   \item{\code{scale_RdBu}}{
#'   A colour gradient for difference heatmaps, from red over white to blue
#'   }
#'   \item{\code{scale_Spectral}}{
#'   A colour gradient for difference heatmaps, from red over yellow to blue
#'   }
#' }
#' 
#' @seealso \code{\link{scale_fill_gradientn}}
#' 
#' @name movhmcolgradients
NULL

#' @export
#' @rdname movhmcolgradients
scale_RdGrBu <- colorRampPalette(
  c("#1639fa", "#2150fa", "#3275fb", "#459afb", "#55befb", "#67e1fc", "#72faf5",
    "#72f8d2", "#72f7ad", "#70f55f", "#70f55f", "#70f538", "#74f52f", "#86f631",
    "#9ff633", "#bbf835", "#d9f938", "#f6fa3b", "#fae238", "#f5be31", "#f19b2c",
    "#ee7627", "#ec5223",  "#eb3b22"))(100)

#' @import RColorBrewer
#' @export
#' @rdname movhmcolgradients
scale_Blues <- colorRampPalette(brewer.pal(9, "Blues"), bias = 1.5)(100)

#' @import RColorBrewer
#' @export
#' @rdname movhmcolgradients
scale_RdBu <- colorRampPalette(brewer.pal(9, "RdBu"))(100)

#' @import RColorBrewer
#' @export
#' @rdname movhmcolgradients
scale_Spectral <- colorRampPalette(brewer.pal(9, "Spectral"))(100)