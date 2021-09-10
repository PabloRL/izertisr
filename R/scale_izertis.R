#' Izertis theme palette
#'
#' This function outputs n colours from the fira ggplot2 theme palette
#'
#' @param n the number of colours to output
#'
izertisPalette <- function(n) {
  if (n <= 4) return(izertis_palette[1:n])
  grDevices::colorRampPalette(izertis_palette[c(2,1,3)], space = "Lab")(n)
}

#' Izertis discrete colour scales
#'
#' Fill scales belonging to the izertis theme
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @param continuous whether the associated variable should be considered
#' continuous. Typically used after "Error: Continuous value supplied to
#' discrete scale"
#'
#' @seealso \code{\link{firaPalette}}
#'
#' @rdname scale_izertis
#' @export
scale_fill_izertis <- function(..., continuous = FALSE) {
  
  if (continuous) {
    
    ggplot2::scale_fill_gradientn(..., colours = izertisPalette(256))
  }else{
    
    ggplot2::discrete_scale("fill", "izertis", izertisPalette, ...)
    
  }
  
}


#' Izertis discrete colour scales
#'
#' Colour scales belonging to the izertis theme
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @param continuous whether the associated variable should be considered
#' continuous. Typically used after "Error: Continuous value supplied to
#' discrete scale"
#'
#' @seealso \code{\link{firaPalette}}
#'
#' @rdname scale_izertis
#' @export
scale_colour_izertis <- function(..., continuous = FALSE) {
  
  
  if (continuous) {
    
    ggplot2::scale_color_gradientn(..., colours = izertisPalette(256))
    
  }else{
    
    ggplot2::discrete_scale("colour", "izertis", izertisPalette, ...)
    
  }
}


#' @rdname scale_izertis
#' @export
scale_color_izertis <- scale_colour_izertis

