# get_time_range ---------------------------------------------------------
##' @name get_time_range
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
##' @export
##' 
##' 


get_time_range <- function(musica.list) {
  range(get_dim_value(musica.list[[1]], "time"))
}


# get_6month ---------------------------------------------------------
##' @name get_6month
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
##' @export
##' 
##' 

get_6month <- function(musica.list) {
  this.range <- get_time_range(musica.list)
  this.min <- this.range[1]
  this.max <- min(this.range[2], this.range[1] + 24*3600*180)
  c(this.min, this.max)
}

# get_soil_level ---------------------------------------------------------
##' @name get_soil_level
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
##' @export
##' 
##' 


get_soil_level <- function(x, soil.level) {
  zsoil <- get_variable(x, "z_soil") %>% 
    filter(nsoil == soil.level) %>% 
    pull(z_soil)
  dzsoil <- get_variable(x, "dz_soil") %>% 
    filter(nsoil == soil.level) %>% 
    pull(dz_soil)
  zsoil + c(-dzsoil/2, dzsoil/2)
}

# get_air_level ---------------------------------------------------------
##' @name get_air_level
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
##' @export
##' 
##' 


get_air_level <- function(x, air.level) {
  veget_top <- get_variable(x, "veget_height_top") %>% 
    pull(veget_height_top) %>% 
    first()
  zair <- get_variable(x, "relative_height") %>% 
    filter(nair == air.level) %>% 
    pull(relative_height) * veget_top
  dzair <- get_variable(x, "layer_thickness") %>% 
    filter(nair == air.level) %>% 
    pull(layer_thickness)
  zair + c(-dzair/2, dzair/2)
}
