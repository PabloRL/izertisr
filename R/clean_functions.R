#' Limpieza segun recorrido intercuantilico
#'
#' @param x vector a limpiar
#' @param k factor que determina la amplitud de la limpieza (k veces 
#' el recorrido intercuantilico)
#' @param qmin cuantil inferior a utilizar
#' @param qmax cuantil superior a utilizar
#'
#' @return x
#' @export
#'
#' @examples clean_numeric_iqr(rnorm(0,50))
clean_numeric_iqr <- function(x , k=1.5 , qmin = 0.25 , qmax = 0.75 , ...){
  
  qi <- quantile(x,qmin,...)
  qs <- quantile(x,qmax,...)
  iqr <- qs - qi
  li <- qi - k*iqr
  ls <- qs + k*iqr
  
  x[x > ls | x < li] <- NA
  
  return(x)
}