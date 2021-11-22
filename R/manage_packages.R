
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
  

 
  
  pkgs_to_download <- pkgs[!(pkgs %in% row.names(installed.packages()) | packages_are_downloaded(path,pkgs))]
  
  
  while (length(pkgs_to_download) > 0 ) {
    
  
    if (!dir.exists(path))
      dir.create(path)

    dowloaded_packages_data <- download.packages(pkgs_to_download,path)
    
    lapply(dowloaded_packages_data[,2],untar)
    
    pkgDescFile <- sapply(pkgs_to_download, function(pkg) paste0(pkg,"/DESCRIPTION"))
    
    DEPS_VERSIONS <- lapply(pkgDescFile, function(file) {
      
      theseDeps <- combineDcfFields(as.data.frame(readDcf(file)), fields)
      
      splitDeps <- lapply(theseDeps, function(x) {
        if (is.na(x)) return(NULL)
        splat <- unlist(strsplit(x, ",[[:space:]]*"))
        ## Remove versioning information as this function only returns package names
        splat <- gsub("\\(.*", "", splat, perl = TRUE)
        gsub("[[:space:]].*", "", splat, perl = TRUE)
      })
      
      splitVers <- lapply(theseDeps, function(x) {
        if (is.na(x)) return(NULL)
        splat <- unlist(strsplit(x, ",[[:space:]]*"))
        ## Remove name information as this function only returns package version
        splat_out <- gsub("(^.*\\([^0-9]*)([0-9\\.]*)(.*)", "\\2",splat)
        splat_out[!grepl("\\(>.*[0-9\\.]*\\)",splat)] <- "0"
        gsub("[[:space:]].*", "", splat_out, perl = TRUE)
      })
      
      data.frame(DEPS = unlist(splitDeps, use.names = FALSE),
                 VERS = unlist(splitVers, use.names = FALSE),
                 stringsAsFactors = FALSE)
    })
    
    DEPS_VERSIONS <- do.call("rbind",DEPS_VERSIONS) 
    
    DEPS_VERSIONS <- DEPS_VERSIONS[order(DEPS_VERSIONS$VERS,decreasing = TRUE),]
    
    DEPS_VERSIONS <- DEPS_VERSIONS[!duplicated(DEPS_VERSIONS$DEPS),]
    
    deps_version <- DEPS_VERSIONS[!areBasePackages(DEPS_VERSIONS$DEPS),]
    
    lapply(pkgs_to_download,unlink,recursive = TRUE)
    
    
    pkgs_to_download <- deps_version$DEPS[!(packages_are_installed(deps_version$DEPS,deps_version$VERS) | packages_are_downloaded(path,deps_version$DEPS,deps_version$VERS))]
    
    
    
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
  
  downloaded_packages_file_complete <- list.files(path, full.names = TRUE)
  
  downloaded_packages_file <- list.files(path, full.names = FALSE)
  
  downloaded_packages_name <- gsub("(^.*)(_.*)","\\1",downloaded_packages_file)
  
  downloaded_packages_version <- gsub("(^.*_)(.*)(\\.tar\\.gz)","\\2",downloaded_packages_file)
  
  downloaded_packages <- data.frame(name = downloaded_packages_name, version = downloaded_packages_version, file = downloaded_packages_file_complete, stringsAsFactors = FALSE)
  
  downloaded_packages <- downloaded_packages[order(downloaded_packages$version, decreasing = TRUE),]
  
  downloaded_packages <- downloaded_packages[!duplicated(downloaded_packages$name),]
  
  
  packages_to_install_file <- downloaded_packages$file[!packages_are_installed(downloaded_packages$name,downloaded_packages$version)]
  
  
  
  while (length( packages_to_install_file) > 0 ) {
    
    browser()
    
    
    lapply(packages_to_install_file, install.packages, repos = NULL)
    
    packages_to_install_file <- downloaded_packages$file[!packages_are_installed(downloaded_packages$name,downloaded_packages$version)]
    
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
    unlist(
      strsplit(
        gsub("^\\s*", "", as.character(dcfFrame[[field]])), "\\s*,\\s*"))
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

areBasePackages <- function(packages) {
  pkgs <- discoverBaseRecommendedPackages()
  packages %in% c("R", pkgs$base)
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



packages_are_downloaded <- function(path, pkgs, versions = "0") {
  
  
  
  
  pkgs_downloaded <- list.files(path, full.names = FALSE, recursive = FALSE)
  
  mapply(function(pkcs,version){
    
    positions <- grep(pkcs,pkgs_downloaded)
    
    if (length(positions) == 0) {
      
      return(FALSE)
      
    } else {
      
      versions <- gsub("(^.*_)(.*)(\\.tar\\.gz)","\\2",pkgs_downloaded)
      
      recent_version <- max(versions[positions])
      
      return(recent_version >= version)
      
    }
  }, pkgs, versions, SIMPLIFY = TRUE)
    
  

}

packages_are_installed <- function(pkgs, versions = "0") {
  

  
  pkgs_installed <- as.data.frame.matrix(installed.packages(),stringsAsFactors = FALSE)
  
  mapply(function(pkg,version){
    
    
    positions <- which(pkg == pkgs_installed$Package)
    
    if (length(positions) == 0) {
      
      return(FALSE)
      
    } else {
      
      versions <- pkgs_installed$Version[positions]
      
      recent_version <- max(versions)
      
      return(recent_version >= version)
      
    }
  }, pkgs, versions, SIMPLIFY = TRUE)
  
  
  
}



