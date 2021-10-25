
#' Descarga paquetes y sus dependencias para poder instalarlos en una máquina sin conexión a internet
#'
#' @param pkgs vector con los nombres de los paquetes a instalar
#' @param path dirección donde se guardan los paquetes descargados
#' @param fields tipo de dependencias a descargar
#'
#' @return
#' @export
#'
#' @examples download_packages_and_deps("devtools")
download_packages_and_deps <- function(pkgs,
                                      path = "extrapackages/",
                                      fields = c("Depends","Imports","LinkingTo")){
  

  
  pkgs_to_download <- pkgs[!(pkgs %in% row.names(installed.packages()) | packages_are_downloaded(pkgs,path))]
  
  
  while (length(pkgs_to_download) > 0 ) {
    
  
    if (!dir.exists(path))
      dir.create(path)
    
    
    dowloaded_packages_data <- download.packages(pkgs_to_download,path)
    
    lapply(dowloaded_packages_data[,2],untar)
    
    pkgDescFile <- sapply(pkgs_to_download, function(pkg) paste0(pkg,"/DESCRIPTION"))
    
    DEPS <- lapply(pkgDescFile, function(file) {
      
      theseDeps <- combineDcfFields(as.data.frame(readDcf(file)), fields)
      
      splitDeps <- lapply(theseDeps, function(x) {
        if (is.na(x)) return(NULL)
        splat <- unlist(strsplit(x, ",[[:space:]]*"))
        ## Remove versioning information as this function only returns package names
        splat <- gsub("\\(.*", "", splat, perl = TRUE)
        gsub("[[:space:]].*", "", splat, perl = TRUE)
      })
      
      unlist(splitDeps, use.names = FALSE)
    })
    
    deps <- unique(unlist(DEPS))
    deps <- dropSystemPackages(deps)
    
    lapply(pkgs_to_download,unlink,recursive = TRUE)
    
        
    pkgs_to_download <- deps[!(deps %in% row.names(installed.packages()) | packages_are_downloaded(deps,path))]
    
    
    
  }
  
}


#' Instala paquetes locales teniendo en cuenta que están todas las dependencias disponibles
#'
#' @param path  dirección de los paquetes a instalar
#'
#' @return
#' @export
#'
#' @examples install_locale_packages()
install_locale_packages <- function(path = "extrapackages/") {
  
  downloaded_packages_file <- list.files(path, full.names = TRUE)
  
  downloaded_packages_name <- gsub("(^.*)(_.*)","\\1",list.files(path))
  
  packages_to_intall_file <- downloaded_packages_file[!(downloaded_packages_name %in% row.names(installed.packages()))]
  
  
  while (length( packages_to_intall_file) > 0 ) {
    
    lapply(packages_to_intall_file, install.packages, repos = NULL)
    
    packages_to_intall_file <- downloaded_packages_file[!(downloaded_packages_name %in% row.names(installed.packages()))]
    
  }
  
  
}





# wrapper around read.dcf to workaround LC_CTYPE bug
# (see: http://r.789695.n4.nabble.com/Bug-in-read-dcf-all-TRUE-td4690578.html)
readDcf <- function(...) {
  loc <- Sys.getlocale('LC_CTYPE')
  on.exit(Sys.setlocale('LC_CTYPE', loc))
  read.dcf(...)
}

# Combines one or more comma-delimited fields from a data frame read from a
# DCF.
combineDcfFields <- function(dcfFrame, fields) {
  unique(unlist(lapply(fields, function(field) {
    gsub("\\s.*", "", unlist(
      strsplit(
        gsub("^\\s*", "", as.character(dcfFrame[[field]])), "\\s*,\\s*")))
  })))
}



discoverBaseRecommendedPackages <- function() {
  
  # First, attempt to ask 'tools' what the standard package
  # names are. Since this function is unexported we are
  # careful on how we query + use it.
  tools <- asNamespace("tools")
  pkgs <- tryCatch(tools$.get_standard_package_names(), error = identity)
  ok <- is.list(pkgs) &&
    all(c("base", "recommended") %in% names(pkgs)) &&
    length(pkgs$base) &&
    length(pkgs$recommended)
  if (ok)
    return(pkgs)
  
  # Otherwise, fall back to installed.packages().
  ip <- utils::installed.packages()
  list(
    base        = rownames(ip)[ip[, "Priority"] %in% "base"],
    recommended = rownames(ip)[ip[, "Priority"] %in% "recommended"]
  )
  
}


excludeBasePackages <- function(packages) {
  pkgs <- discoverBaseRecommendedPackages()
  setdiff(packages, c("R", pkgs$base))
}



dropSystemPackages <- function(packages) {
  
  # always exclude base packages
  packages <- excludeBasePackages(packages)
  
  
  packages
}



packages_are_downloaded <- function(pkgs, path) {
  
  pkgs_downloaded <- list.files(path, full.names = FALSE, recursive = FALSE)
  
  pkgs %in% gsub("(^.*)(_.*)","\\1",pkgs_downloaded)
}





