library(dplyr)
library(ggplot2)
library(RColorBrewer)

scale_RdGrBu <- colorRampPalette(
  c("#1639fa", "#2150fa", "#3275fb", "#459afb", "#55befb", "#67e1fc", "#72faf5",
    "#72f8d2", "#72f7ad", "#70f55f", "#70f55f", "#70f538", "#74f52f", "#86f631",
    "#9ff633", "#bbf835", "#d9f938", "#f6fa3b", "#fae238", "#f5be31", "#f19b2c",
    "#ee7627", "#ec5223",  "#eb3b22"))(100)

scale_Blues <- colorRampPalette(brewer.pal(9, "Blues"), bias = 1.5)(100)

scale_RdBu <- colorRampPalette(brewer.pal(9, "RdBu"))(100)

scale_Spectral <- colorRampPalette(brewer.pal(9, "Spectral"))(100)

movhm <- function(l, blocksize, margins, origin = NULL, consider.time = FALSE,
                  print = FALSE)
{
  # Create raster data frame for each Ss
  raster <- lapply(l, count.pos, blocksize = blocksize, margins = margins,
                   consider.time = consider.time)
  
  # Summarise all raster data frames
  raster <- Reduce(function(x, y){ x$f <- x$f + y$f; return(x) }, raster)
  
  # Set cells with zero visits to NA (to plot these cells transparent)
  raster[raster$f == 0, "f"] <- NA
  
  # Set start position to NA
  if (!is.null(origin))
  {
    raster[raster$x == origin[1] / blocksize &
             raster$y == origin[2] / blocksize, "f"] <- NA
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

movhm.diff <- function(lx, ly, difference = "relative", blocksize, margins,
                       origin = NULL, consider.time = FALSE, print = FALSE)
{
  raster.x <- movhm(lx, blocksize = blocksize, margins = margins,
                    origin = origin, consider.time = consider.time)
  raster.y <- movhm(ly, blocksize = blocksize, margins = margins,
                    origin = origin, consider.time = consider.time)
  
  if (difference == "relative")
  {
    raster <- raster.x
    raster$f <- raster.x$f / length(lx) - raster.y$f / length(ly)
  } else if (difference == "absolute")
  {
    raster <- raster.x
    raster$f <- raster.x$f <- raster.y$f
  } else
  {
    stop("Invalid input for parameter difference.")
  }
  
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

count.pos <- function(x, blocksize, margins, consider.time = FALSE)
{
  # Set column names
  names(x) <- c("x", "y")
  
  # Apply blocksize, round values and delete values out of borders
  data <- round(x / blocksize) %>%
    filter(x > margins[1] / blocksize, x < margins[3] / blocksize,
           y > margins[2] / blocksize, y < margins[4] / blocksize)
  
  if (consider.time)
  {
    # Count the number of times Ss was in each cell
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
  raster <- expand.grid(
    list(x = seq(margins[1] / blocksize, margins[3] / blocksize),
         y = seq(margins[2] / blocksize, margins[4] / blocksize))
  )
  
  # Add values to raster data frame
  raster <- merge(raster, data, all = TRUE)
  
  # Set NA to 0 (is needed to summarise multiple raster data frames)
  raster[is.na(raster$f), "f"] <- 0
  
  # Return raster data frame
  raster
}

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

# template  a ggplot2 object
# margins   margins of plot
# blocksize side length of blocks (default: 50)
# roi       a data-frame containing roi specification (optional)
show.me.da.raster <- function(template, margins, blocksize, roi = NULL)
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

# # berechnet in wie vielen zellen der roi sich eine person aufgehalten hat
# roi.cells.visited <- function(x, roi, margins, blocksize)
# {
#   # Compute raster data frame for Ss
#   raster <- count.pos(x, blocksize = blocksize, margins = margins)
#   
#   # Count number of cells which occur in both roi and raster w/ f == 1
#   nrow(inner_join(roi, filter(raster, f == 1)))
# }

roi.analysis <- function(..., roi, blocksize, margins)
{
  objects <- list(...)
  
  if (length(objects) == 0)
  {
    stop("You need to specify at least one list of data frames to ...")
  }
  
  lapply(objects, sapply, function(x) {
    # Compute raster data frame for Ss
    raster <- count.pos(x, blocksize = blocksize, margins = margins)
    
    # Count number of cells which occur in both roi and raster w/ f == 1
    nrow(inner_join(roi, filter(raster, f == 1)))
  })
}