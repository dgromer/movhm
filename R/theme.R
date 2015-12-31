#' A blank theme for heatmaps
#' 
#' @importFrom ggplot2 theme_bw theme element_blank
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
