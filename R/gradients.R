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
#'   \item{\code{scale_RdBu_bias(bias = 1.5)}}{
#'   A function for creating a colour gradient for difference heatmaps, from red
#'   over white to blue (like scale_RdBu). The \code{bias} parameter defines
#'   how widely the colours space towards both ends.
#'   }
#'   \item{\code{scale_Spectral}}{
#'   A colour gradient for difference heatmaps, from red over yellow to blue
#'   }
#'   \item{\code{scale_Spectral_bias(bias = 1.5)}}{
#'   A function for creating a colour gradient for difference heatmaps, from red
#'   over yellow to blue (like scale_Spectral). The \code{bias} parameter
#'   defines how widely the colours space towards both ends.
#'   }
#' }
#'
#' @seealso \code{\link{scale_fill_gradientn}}
#'
#' @name movhmcolgradients
NULL

#' @export
#' @rdname movhmcolgradients
scale_RdGrBu <- colorRampPalette(rainbow(12, end = .65))(100)

#' @importFrom RColorBrewer brewer.pal
#' @export
#' @rdname movhmcolgradients
scale_Blues <- colorRampPalette(brewer.pal(9, "Blues"), bias = 1.5)(100)

#' @importFrom RColorBrewer brewer.pal
#' @export
#' @rdname movhmcolgradients
scale_RdBu <- colorRampPalette(brewer.pal(11, "RdBu"))(100)

#' @importFrom RColorBrewer brewer.pal
#' @export
#' @rdname movhmcolgradients
scale_RdBu_bias <- function(bias = 1.5)
{
  c(rev(colorRampPalette(brewer.pal(9, "Reds"), bias = bias)(50)),
    colorRampPalette(brewer.pal(9, "Blues"), bias = bias)(50))
}

#' @importFrom RColorBrewer brewer.pal
#' @export
#' @rdname movhmcolgradients
scale_Spectral <- colorRampPalette(brewer.pal(11, "Spectral"))(100)

#' @importFrom RColorBrewer brewer.pal
#' @export
#' @rdname movhmcolgradients
scale_Spectral_bias <- function(bias = 1.5)
{
  c(rev(colorRampPalette(brewer.pal(9, "YlOrRd"), bias = bias)(50)),
    colorRampPalette(brewer.pal(9, "YlGnBu"), bias = bias)(50))
}
