#' ROI Class
#' 
#' @docType class
#' @section Methods
#' \describe{
#'   \item{\code{add(xmin, ymin, xmax, ymax)}}{Add an area to the ROI}
#'   \item{\code{plot(bg = NULL)}}{Plot the current ROI (on top of an optional
#'   background)}
#' }
#' @importFrom R6 R6Class
#' @examples 
#' # Create a new roi with a bin width of 1
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
   
   roi = data.frame(x = numeric(0), y = numeric(0)),
   binwidth = NULL,
   
   initialize = function(bin_width)
   {
     if (!missing(bin_width)) self$bin_width <- bin_width
   },
   
   # Add a rectangle to the current roi
   add = function(xmin, ymin, xmax, ymax)
   {
     x <- seq(xmin, xmax, self$bin_width)
     y <- seq(ymin, ymax, self$bin_width)
     
     nreg <- data.frame(x = rep(x, each = length(y)), y = rep(y, length(x)))
     
     self$roi <- full_join(self$roi, nreg, by = c("x", "y"))
     
     invisible(self)
   },
   
   print = function()
   {
     print(self$roi)
   },
   
   plot = function(bg = NULL)
   {
     # If no background was passed, use an empty ggplot2 object
     if (is.null(bg))
     {
       bg <- ggplot()
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

#' @importFrom purrr map
#' @export
roi_analysis <- function(roi, ..., x, y, time = TRUE)
{
  map(list(...), roi_analysis_, x, y, roi$roi, roi$bin_width, time)
}

#' @importFrom dplyr filter inner_join
#' @importFrom purrr map
roi_analysis_ <- function(l, x, y, roi, bin_width, time)
{
  bins <- map(l, bin_2d, x, y, bin_width)
  
  if (time)
  {
    # Filter rows which occur in both roi and raster w/ f > 0, then sum total f
    map_dbl(bins, ~ sum(inner_join(roi, filter(.x, f > 0), by = c("x", "y"))$f))
  }
  else
  {
    # Count number of cells which occur in both roi and raster w/ b == 1
    map_dbl(bins, ~ nrow(inner_join(roi, filter(.x, b == 1), by = c("x", "y"))))
  }
}
