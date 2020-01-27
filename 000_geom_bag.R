#'  Bagplot
#' 
#' The bag geom is useful for graphical summaries of scatterplots. It 
#' is effective at showing the  location, spread, skewness, and 
#' outliers of a data set.
#'  
#' A bagplot is a bivariate generalization of the well known boxplot. It
#' was proposed by Rousseeuw, Ruts, and Tukey. This geom plots bagplots that 
#' are very similar to the one described in Rousseeuw et al. and
#' uses code from their bagplot functions in the aplpack pacakge. 
#' 
#' A bagplot adds three features to a scatterplot. First is the depth 
#' median, this is the point with the highest possible Tukey depth. It is
#' analogous to (but not identical to) to the common univariate median. 
#' Second is a polygon that encloses 50% of the points around the depth
#' median. This is called the 'bag', and is analgous to the box in a box 
#' and whisker plot. Third is a polygon that is a convex hull around the
#' points inside a region that is 3 times the size of the bag. This is
#' called the 'loop', and is analagous to the whiskers on a box 
#' and whisker plot. Points located outside of the loop are outliers, 
#' similar to outliers on a a box and whisker plot.
#'
#' Note that bagplots are not very informative for small datasets (n < 10),
#' and they may be time-consuming and memory-intensive to compute for
#' large datasets (n > 10,000). They are also not suitable for multi-modal
#' datasets, for which a plot that shows Bivariate Highest Density Regions
#' should be used (such as in the hdrcde package). 
#' 
#' 
#' Rousseeuw, P., Ruts, I. Tukey, J. The bagplot: a bivariate boxplot. 
#' The American Statistician, Vol. 53 Num. 4 (1999) 382-387 http://venus.unive.it/romanaz/ada2/bagplot.pdf
#' 
#' Wickham H, Stryjewski L. 40 years of boxplots. http://vita.had.co.nz/papers/boxplots.html

#' Geom Proto
#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
GeomBag <- ggproto(
  "GeomBag",
  Geom,
  setup_data = function(self, data, params) {
    data <- ggproto_parent(GeomPolygon, self)$setup_data(data, params)
    data
  },
  
  draw_group = function(data, panel_scales, coord) {
    n <- nrow(data)
    if (n <= 2)
      return(grid::nullGrob())
    
    # custom bits, thanks to Baptise Baptiste Auguié for his answer: http://stackoverflow.com/a/36163885/1036500
    
    # prepare data to go into function below
    the_matrix <- suppressWarnings(data.matrix(cbind(data$x, data$y)))
    
    # compute the bag and loop coords
    hulls <- hulls_for_bag_and_loop(the_matrix)
    
    # close the loop by repeating the first coord at the end
    hulls_closed <- lapply(hulls[1:2], function(i) data.frame(rbind(i, i[1, ] ), row.names = NULL ))
    
    # extract loop and hull coords
    the_loop <-  setNames(data.frame(hulls_closed$hull.loop), nm = c("x", "y"))
    the_bag <-   setNames(data.frame(plothulls_(the_matrix, fraction = 0.5)), nm = c("x", "y"))
    
    # put the groups, and other attributes in data, back on
    the_loop_etc <- data[match(interaction( the_loop$x, the_loop$y),
                               interaction( data$x, data$y)
    ), ]
    
    # put the groups, and other attributes in data, back on
    the_bag_etc <- data[match(interaction( the_bag$x, the_bag$y),
                              interaction( data$x, data$y)
    ), ]
    
    # get the center, which are new coords not in the original dataset
    center <-  setNames(data.frame(matrix(hulls$center, nrow = 1)), nm = c("center_x", "center_y"))
    # join the center with other attributes in data, just the first row
    center <- data.frame(cbind(x = center$center_x,
                               y = center$center_y,
                               data[1, -which(names(data) %in% c('x', 'y'))]))
    
    coords <- coord$transform(center, panel_scales)     
    
    # set alpha
    the_loop_etc$alpha <-  0.1
    the_bag_etc$alpha <-   0.2
    
    # set the grobs 
    loop_grob <- GeomPolygon$draw_panel(the_loop_etc, panel_scales, coord)
    bag_grob <- GeomPolygon$draw_panel(the_bag_etc, panel_scales, coord)
    center_grob <- grid:::pointsGrob(x = coords$x, 
                                     y = coords$y, 
                                     default.units = "npc", 
                                     pch = "+",
                                     gp=grid:::gpar(cex = 1.5,  col = coords$colour))
    
    
    # end of custom bits
    
    ggplot2:::ggname("geom_bag",
                     grid:::grobTree(loop_grob ,
                                     bag_grob,
                                     center_grob
                     ))
    
  },
  
  required_aes = c("x", "y"),
  
  draw_key = draw_key_polygon, 
  
  default_aes = aes(
    colour = "grey80",
    fill = "grey80",
    size = 0.5,
    linetype = 1,
    alpha = 0.5
  )
)

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
geom_bag <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    layer(
      geom = GeomBag,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }
