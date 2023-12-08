# shinymusica ---------------------------------------------------------
##' @name shinymusica
##' @author Remi Lemaire-Patin
##' 
##' @title plot for a single variable
##' 
##' @description This function plot the output of \code{\link{get_variable}}
##' 
##' 
##' @param df a \code{data.frame} object
##' 
##' @return
##' 
##' A \code{ggplot} object
##' 
##' @family plot
##'   
##' @examples
##' library(ncdf4)
##' 
##' 
##' @importFrom shiny shinyApp
##' @export
##' 
##' 


shinymusica <- function(x) {
  shinyApp(musica_ui(x), musica_server(x))
}