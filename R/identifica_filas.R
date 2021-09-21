#' Funcion que identifica tramos con un incremento constante dado
#'
#' @param x 
#' @param incremento 
#'
#' @return
#' @export
#'

ifilas_tramos <- function(x,incremento){
  
  
  inc <- c(incremento,diff(x))
  
  expresion <- paste0("inc","!=",incremento)
  
  cambio.tramo <- which(eval(parse(text = expresion)))
  
  cambio.tramo <- c(1,cambio.tramo, length(x) + 1)
  
  as.character(rep(1:length(diff(cambio.tramo)), diff(cambio.tramo)))
  
  
}



#' Funcion que identifica tramos con un incremento contante dado en tiempo en segundo
#'
#' @param x 
#' @param incremento 
#'
#' @return
#' @export
#'
ifilas_tramos_tiempo <- function(x,incremento){
  
  
  inc <- c(incremento,diff(as.numeric(x)))
  
  expresion <- paste0("inc","!=",incremento)
  
  cambio.tramo <- which(eval(parse(text = expresion)))
  
  cambio.tramo <- c(1,cambio.tramo, length(x) + 1)
  
  as.character(rep(1:length(diff(cambio.tramo)), diff(cambio.tramo)))
  
  
}



#' Funcion que identifica tramos con un incremento no superior a un tiempo en segundos
#'
#' @param x 
#' @param incremento 
#'
#' @return
#' @export
#'
ifilas_tramos_tiempo_sup <- function(x,incremento){
  
  
  inc <- c(incremento,diff(as.numeric(x)))
  
  expresion <- paste0("inc",">",incremento)
  
  cambio.tramo <- which(eval(parse(text = expresion)))
  
  cambio.tramo <- c(1,cambio.tramo, length(x) + 1)
  
  as.character(rep(1:length(diff(cambio.tramo)), diff(cambio.tramo)))
  
  
}



#' Con esta función se pretende agrupar con un indice tramos que compartan alguna característica en común
#'
#' @param x 
#' @param condition 
#' @param fill 
#'
#' @return
#' @export
#'
#' @examples
identifica_filas <- function(x,condition,fill=NA){
  
  # Paso a texto la codicion completa
  condition.text <- paste0("x",condition)
  #Extraigo el número de filas que cumplen la condición
  id.fila <- which(eval(parse(text = condition.text)))
  #Calculo los tramos contiguos
  tramo <- tramos(id.fila,1)
  # El vector que devuelve la funcion debe ser del mismo tamaño que el vector x. las filas que no cumplen la condicion seran rellenadas por
  #fill
  out <- as.character(rep(fill,length(x)))
  #Añado los tramos identificados
  out[id.fila] <- tramo
  out
  
  
}



#' Cuenta repeticiones y extrae valores repetido a lo largo de un vector
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
cuenta_repeticiones_consecutivas <- function(x){
  
  if ( all(is.na(x)) ) {
    repeticiones <- 0
    x_rep_max <- NA
  }else{
    dif = c(0,diff(x))
    tramos <- tramos(dif,0)
    table_rep_max <- which.max(table(tramos))
    repeticiones <- as.numeric(table_rep_max)
    tramo_id <- as.numeric(attr(table_rep_max,"names"))
    x_rep_max <- first(unique(x[tramos == tramo_id]))
  }
  return(c(repeticiones = repeticiones, valor_encontrado = x_rep_max))
  
}