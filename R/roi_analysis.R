#' ROI Class
#' 
#' Class for defining a ROI (area of interest) for use with \code{roi_analysis}.
#' 
#' @importFrom dplyr full_join
#' @importFrom ggplot2 aes ggplot geom_tile geom_hline geom_vline coord_fixed
#'   theme_classic
#' @importFrom R6 R6Class
#' @docType class
#' @usage \code{ROI$new(bin_width)}
#' @format A \code{\link{ROI}} generator object
#' @section Methods:
#' \describe{
#'   \item{\code{add(xmin, ymin, xmax, ymax)}}{Add a region to the ROI.}
#'   \item{\code{plot(bg = NULL)}}{Plot the ROI (to an optional background).}
#' }
#' @examples 
#' # Create a new ROI with a bin width of 1
#' roi <- ROI$new(1)
#' 
#' # Add the area from 0/0 to 2/4 to the ROI and view it
#' roi$add(0, 0, 2, 4)
#' roi$plot()
#' 
#' # Increase the roi
#' roi$add(2, 2, 4, 4)
#' roi$plot()
#' @export
ROI <- R6Class("ROI",
               
  public = list(
    
    # roi holds the positions of the ROI as a data frame where each line
    # represents one cell of the ROI
    roi = data.frame(x = numeric(0), y = numeric(0)),
    
    # Width of the bins
    bin_width = NULL,
   
    initialize = function(bin_width)
    {
      self$bin_width <- bin_width
    },
   
    # Add a rectangle to the current roi
    add = function(xmin, ymin, xmax, ymax)
    {
      # Round coordinates to bin width
      xmin <- round_bins(xmin, self$bin_width)
      ymin <- round_bins(ymin, self$bin_width)
      xmax <- round_bins(xmax, self$bin_width)
      ymax <- round_bins(ymax, self$bin_width)
      
      # Sequence of x- and y-values defining the border of the added rectangle
      x <- seq(xmin, xmax, self$bin_width)
      y <- seq(ymin, ymax, self$bin_width)
      
      # New area as data frame of x- and y-values
      nreg <- data.frame(x = rep(x, each = length(y)), y = rep(y, length(x)))
      
      # Add to existing roi
      self$roi <- full_join(self$roi, nreg, by = c("x", "y"))
      
      invisible(self)
    },
    
    print = function()
    {
      print(private$roi)
    },
    
    plot = function(bg = NULL)
    {
      # If no background was passed, use an empty ggplot2 object
      if (is.null(bg))
      {
        bg <- ggplot()
      }
      else if (!inherits(bg, "ggplot"))
      {
        stop("'bg' must be of class ggplot")
      }
      
      # Calculate positions of raster lines
      xs <- seq(min(self$roi$x) - self$bin_width / 2,
                max(self$roi$x) + self$bin_width / 2, self$bin_width)
      ys <- seq(min(self$roi$y) - self$bin_width / 2,
                max(self$roi$y) + self$bin_width / 2, self$bin_width)
      
      print(
        bg +
          geom_tile(aes(x, y), self$roi, alpha = .25, fill = "#373b41") +
          geom_vline(xintercept = xs, color = "#c5c8c6") +
          geom_hline(yintercept = ys, color = "#c5c8c6") +
          coord_fixed() +
          theme_classic()
      )
    }
  )
)

#' ROI analysis for group movement data
#' 
#' @importFrom purrr map
#' @export
roi_analysis <- function(roi, data, x, y, time = TRUE)
{
  if (!inherits(roi, c("ROI", "R6")))
  {
    stop("Argument 'roi' must be of class ROI")
  }
  
  map(data, roi_analysis_, x, y, roi$roi, roi$bin_width, time)
}

#' @importFrom dplyr filter inner_join
#' @importFrom purrr map
roi_analysis_ <- function(l, x, y, roi, bin_width, time)
{
  margins <- find_margins(l, x, y, bin_width)
  
  bins <- map(l, bin_2d, x, y, bin_width, margins, time)
  
  if (time)
  {
    # Filter rows which occur in both roi and raster w/ f > 0, then sum total f
    map_dbl(bins, ~ sum(inner_join(roi, filter(.x, f > 0), by = c("x", "y"))$f))
  }
  else
  {
    # Count number of cells which occur in both roi and raster w/ f == 1
    map_dbl(bins, ~ nrow(inner_join(roi, filter(.x, f == 1), by = c("x", "y"))))
  }
}
