#' Creacion de template html de Izertis
#'
#' @param toc 
#' @param toc_float 
#' 
#' @importFrom rmarkdown html_document includes
#'
#' @return
#' @export
#'
izertis_report <- function(toc = TRUE, toc_float = FALSE, keep_md = FALSE) {

  
  # get the locations of resource files located within the package
  css <- system.file("resources/css/izertis_template.css", package = "izertisr")
  footer <- system.file("resources/footer/izertis_footer.html", package = "izertisr")
  header <- system.file("resources/header/izertis_header.html", package = "izertisr")


  
  # call the base html_document function
  rmarkdown::html_document(toc = toc,
                           keep_md = keep_md,
                           number_sections = TRUE,
                           df_print = "kable",
                           toc_float = toc_float,
                           self_contained = TRUE,
                           fig_width = 15,
                           fig_height = 8,
                           css = css,
                           includes = includes(after_body = footer,
                                               in_header = header))
}

#' Creacion de template html de Redes
#' @param toc 
#' @param toc_float 
#' 
#' @importFrom rmarkdown html_document includes
#'
#' @return
#' @export
#'
redes_report <- function(toc = TRUE, toc_float = FALSE, keep_md = FALSE) {
  
  
  # get the locations of resource files located within the package
  css <- system.file("resources/css/izertis_template.css", package = "izertisr")
  footer <- system.file("resources/footer/izertis_footer.html", package = "izertisr")
  header <- system.file("resources/header/redes_header.html", package = "izertisr")
  
  
  
  # call the base html_document function
  rmarkdown::html_document(toc = toc,
                           keep_md = keep_md,
                           number_sections = TRUE,
                           df_print = "kable",
                           toc_float = toc_float,
                           self_contained = TRUE,
                           fig_width = 15,
                           fig_height = 8,
                           css = css,
                           includes = includes(after_body = footer,
                                               in_header = header))
}
