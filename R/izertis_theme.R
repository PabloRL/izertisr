#' Izertis theme
#' @param  main_color1 color principal 1
#' @param  main_color2 color principal 2
#' @param  secondary_color1 color secondary 1
#' @param  secondary_color1 color secondary 2
#' @import ggplot2
#' @return ggplot theme
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = mpg*0.43, y = wt*0.4535924, colour = factor(cyl))) +
#'   geom_point(size = 2) +
#'   labs(title = "Car weight vs efficiency",
#'        subtitle = "Using sensible metrics",
#'        x = "Efficiency (km/l)",
#'        y = "Weight (1000 kg)",
#'        colour = "Cylinders") +
#'   theme_izertis() 
#'
#' @export
theme_izertis <- function(main_color1 = coral_red,
                          main_color2 = cobalt,
                          secondary_color1 = cream_can,
                          secondary_color2 = mine_shaft) {

  ggplot2::`%+replace%`(
    ggplot2::theme_grey(base_size = 11.5),
    ggplot2::theme(
      # add padding to the plot
      plot.margin = grid::unit(rep(0.5, 4), "cm"),
  
      # remove the plot background and border
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      
      # make the legend and strip background transparent
      legend.background = ggplot2::element_rect(fill = "transparent",
                                                colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent",colour = NA),
      strip.background = ggplot2::element_rect(fill = "transparent",
                                               colour = NA),
      
      # add light, dotted major grid lines only
      panel.grid.major = ggplot2::element_line(linetype = "dotted",
                                               colour = main_color2,
                                               size = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      
      # remove the axis tick marks and hide axis lines
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = secondary_color2, size = 0.3),
      
      # modify the bottom margins of the title and subtitle
      plot.title = ggplot2::element_text(size = 18, colour = main_color1,
                                         hjust = 0.5,
                                         margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = 12, colour = main_color2,
                                            hjust = 0.5,
                                            margin = ggplot2::margin(b = 10)),
      
      # add padding to the caption
      plot.caption = ggplot2::element_text(size = 10, colour = main_color2,
                                           hjust = 1,
                                           margin = ggplot2::margin(t = 15)),
      
      # Adjust text size and axis title position
      axis.title = ggplot2::element_text(size = 13, colour = main_color2,
                                         hjust = 0.95),
      axis.text = ggplot2::element_text(size = 10, colour = secondary_color2),
      legend.title = ggplot2::element_text(size = 12, colour = secondary_color2),
      legend.text = ggplot2::element_text(size = 10, colour = secondary_color2),
      strip.text = ggplot2::element_text(size = 12, colour = secondary_color2, 
                                         margin = ggplot2::margin(10, 10, 
                                                                  10, 10, 
                                                                  "pt"))
    )
  )
}




