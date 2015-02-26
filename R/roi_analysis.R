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
#' @param consider.time logical indicating whether to count one subject multiple
#'   times in one cell
#' @export
roi.analysis <- function(..., x, y, roi, blocksize, margins,
                         consider.time = FALSE)
{
  objects <- list(...)
  
  if (length(objects) == 0)
  {
    stop("You need to specify at least one list of data frames to ...")
  }
  
  # Round roi values due to R's problems with numeric values (see FAQ 7.31)
  if (blocksize < 1)
  {
    roi$x <- round_any(roi$x, blocksize)
    roi$y <- round_any(roi$y, blocksize)
  }
  
  lapply(objects, sapply, function(data) {
    # Compute raster data frame for Ss
    raster <- count.pos(data, x = x, y = y, blocksize = blocksize,
                        margins = margins, consider.time = consider.time,
                        time.transformation = NULL)
    
    if (consider.time)
    {
      # Filter rows which occur in both roi and raster w/ f > 0
      tpos <- inner_join(roi, filter(raster, f > 0), by = c("x", "y"))
      
      # Return sum of time * position frequencies
      sum(tpos$f)
    } else
    {
      # Count number of cells which occur in both roi and raster w/ f == 1
      nrow(inner_join(roi, filter(raster, f == 1), by = c("x", "y")))
    }
  })
}
