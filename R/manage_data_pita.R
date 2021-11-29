## fecha minima de extraccion por defecto
default_date <- "2019-01-01"


#' Refresca datos en cache para una determinada consulra
#'
#' @param connections lista con conexiones a la bbdd postgre, si se requiere fast y slow, primero fast
#' @param query query en cuestion
#' @param id id de la query
#' @param dir_cache directorio donde se guarda el cache
#' 
get_refresh_pita <- function(connections, 
                           query, 
                           id, 
                           dir_cache){
  
  if (!dir.exists(dir_cache))
    dir.create(dir_cache)
  
  date_extraction <- format(Sys.Date(),"%Y_%m_%d")
  
  last_date <- get_last_date(id,dir_cache)

  if ((last_date < date_extraction)) {
    
    data <- dbGetQuery(connections[[1]],query)
    
    dbDisconnect(connections[[1]])
    
    if (last_date == default_date & length(connections) > 1) {
      
    slow_data <- dbGetQuery(connections[[2]],query)
    dbDisconnect(connections[[2]])
    
    data <- rbind(slow_data,data)
      
    }
    
    
    file_name_out <- paste0(dir_cache,date_extraction,"_",id,".rds")
    
    saveRDS(data,file_name_out)
    
    message("Cache actualizado")
    
  }else{
    
    message("El cache ya ha sido actualizado para la fecha actual")
    
  }
  
}

#' Extrae historico de una consulta determinada
#'
#' @param id id del consulta
#' @param dir_cache directorio donde se guarda el cache
#'
#' @export
#'
get_hist_data <- function(id, 
                           dir_cache){
  
  files <- list.files(dir_cache,pattern = id, full.names = TRUE)
  
  if (length(files) > 0) {
    hist_list <- lapply(files,function(file) readRDS(file)) 
      
    hist <- do.call("rbind",hist_list)
    
  }else{
    
    hist <- NULL
  }
  
  return(hist)
  
}


#' Extrae la última fecha descargada en cache para una determinada consulta
#'
#' @param id id de la consulta
#' @param dir_cache directorio donde se guarda el cache
get_last_date <- function(id,
                           dir_cache
                          ){
  
  files <- list.files(dir_cache,pattern = id, full.names = TRUE)
  
  if (length(files) > 0) {
    last_date <- max(gsub(".*([0-9]{4}_[0-9]{2}_[0-9]{2}).*","\\1",files))
  } else {
    last_date <- default_date
  }
  
}


#' Extrae los eventos de pdn de los contadores
#'
#' @param dir_cache directorio donde se guarda el cache
#' @param refresh se refresca
#' 
#' @import RPostgreSQL
#'
#' @export
#'
iget_data_eventos_1_7 <- function(dir_cache = "00Cache_global/",
                                  refresh = TRUE) {
  
  
  pgdrv <- dbDriver(drvName = "PostgreSQL")
   
  connections <- lapply(c(5432,5433), function(port){
    
    dbConnect(pgdrv,
              dbname = "PITA",
              host = "10.243.42.38", 
              port = port,
              user = 'inovgrid',
              password = 'inovgrid_2019') 

  })
  

  
  date_extraction <- format(Sys.Date(),"%Y_%m_%d")
  
  id <- "eventos_1_7"
  
  if (refresh) {
    
    last_date <- get_last_date(id, dir_cache)
    
    last_date_month <- substr(last_date,1,7)
    
    date_extraction_month <- substr(date_extraction,1,7)
    
    table <- paste0("prod.eventos",ifelse(last_date_month == date_extraction_month, paste0("_",date_month),""))
    
  
    
    query <- paste0(  
      "SELECT arrive_date as fecha_evento, equipment_id as equipo_id, device_serial as meter, hash as hash_event
    FROM ", table, " eventos
    WHERE eventos.event_code_id = 7 
      AND eventos.date_key >= '",gsub("_","-",last_date),  
      "' AND origin ='ALARM'")
    
    get_refresh_pita(connections, query, id, dir_cache)
    
  }
  
  return(get_hist_data(id, dir_cache))
  
}


#' Extrae los eventos del algoritmo de predicción de pdn de 
#'
#' @param connections 
#' @param dir_cache 
#' @param refresh 
#'
#' @return
#' @export
#'
#' @examples
iget_data_eventos_prediccion_pdn <- function(dir_cache = "00Cache_global/",
                                             refresh = TRUE){
  
  
  pgdrv <- dbDriver(drvName = "PostgreSQL")
  
  con_fastapolo <- dbConnect(pgdrv,
                            dbname = "PITA",
                            host = "10.243.42.39", 
                            port = 5432,
                            user = 'inovgrid',
                            password = 'inovgrid_2019') 
    
 
  
  date_extraction <- format(Sys.Date(),"%Y_%m_%d")
  
  id <- "eventos_pred_pdn"
  
  if (refresh) {

    last_date <- get_last_date(id, dir_cache)
    
    query <- paste0(  
      "SELECT municipio, acometida, ct_code, ct_name,meters_string,  start_date, end_date ,events
      FROM prod.loss_of_neutral
      WHERE start_date  >= '", gsub("_","-",last_date),"'")
    
    get_refresh_pita(list(con_fastapolo), query, id, dir_cache)
    
  }
  
  return(get_hist_data(id, dir_cache))
  
}

iget_data_eventos_sub_sobre <- function(dir_cache = "00Cache_global/",
                                       refresh = TRUE){

  pgdrv <- dbDriver(drvName = "PostgreSQL")
  
  connections <- lapply(c(5432,5433), function(port){
    
    dbConnect(pgdrv,
              dbname = "PITA",
              host = "10.243.42.38", 
              port = port,
              user = 'inovgrid',
              password = 'inovgrid_2019') 
    
  })
  
  
  date_extraction <- format(Sys.Date(),"%Y_%m_%d")
  
  id <- "eventos_sub_sobre"
  
  if (refresh) {
    
    last_date <- get_last_date(id, dir_cache)
    
    last_date_month <- substr(last_date,1,7)
    
    date_extraction_month <- substr(date_extraction,1,7)
    
    table <- paste0("prod.eventos",ifelse(last_date_month == date_extraction_month, paste0("_",date_month),""))
    
    query <- paste0(  
      "SELECT arrive_date as fecha_evento, equipment_id as equipo_id, device_serial as meter, event_type, hash as hash_event
    FROM ", table, " eventos
    WHERE eventos.event_code_id IN (73, 74, 75, 77, 78, 79, 85, 86, 87, 89, 90, 91)
      AND eventos.date_key >= '", gsub("_","-",last_date),  
      "' AND origin ='ALARM'")
    
    get_refresh_pita(connections, query, id, dir_cache)
    
  }
  
  return(get_hist_data(id, dir_cache))
  
  
}
  
