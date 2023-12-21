# get_dynamic_input ---------------------------------------------------------
##' @name get_dynamic_input
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
##' 
get_dynamic_input <- function(x, this.var, input.name, input.var, multiple = FALSE, hide = FALSE) {
  
  list.var <- var_with_dim(x[[1]], this.var)
  label.var <- switch(this.var,
                      "nsoil" = "Soil layer",
                      "nair" = "Air layer",
                      "nspecies" = "Species",
                      "nveg" = "Vegetation layer",
                      "nleafage" = "Leaf age")
  if (input.var %in% list.var & !hide) {
    return(selectInput(input.name, label = div(style = "font-size:10px", label.var),
                       choices = get_dim_value(x[[1]], this.var),
                       selected = 1,
                       multiple = multiple,
                       width = "50%"))
  } else {
    return(NULL)
  }
}

# get_subset_input ---------------------------------------------------------
##' @name get_subset_input
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
##' 
get_subset_input <- function(x,
                             this.var,
                             input.name, 
                             multiple = FALSE) {
  
  list.var <- var_with_dim(x[[1]], this.var)
  label.var <- switch(this.var,
                      "nsoil" = "Soil layer",
                      "nair" = "Air layer",
                      "nspecies" = "Species",
                      "nveg" = "Vegetation layer",
                      "nleafage" = "Leaf age")
  shinyjs::hidden(
    div(id = input.name,
        selectInput(input.name, label = div(style = "font-size:10px", label.var),
                    choices = get_dim_value(x[[1]], this.var),
                    selected = 1,
                    multiple = multiple,
                    width = "50%")
    ))
}


# toggle_subset_input ---------------------------------------------------------
##' @name toggle_subset_input
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

toggle_subset_input <- function(session, input, index.var, tab, x,
                                hide = FALSE){
  for (this.var in c("nsoil", "nair", "nspecies",
                     "nveg", "nleafage")) {
    this.id <- paste0(tab, "_",this.var,index.var)
    this.inputvar <- paste0(tab, "_var",index.var)
    list.var <- var_with_dim(x[[1]], this.var)
    if (!is.null(this.inputvar) &&
        input[[this.inputvar]] %in% list.var &
        !hide) {
      choices <- get_dim_value(x[[1]], this.var)
      shinyjs::show(id = this.id)
      updateSelectInput(
        session = session,
        inputId = this.id,
        choices = choices,
        selected = first(choices))
    } else {
      reset(id = this.id)
      shinyjs::hide(id = this.id)
    }
  }
}

# get_range_input ---------------------------------------------------------
##' @name get_range_input
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
get_range_input <- function(tab, type) {
  hidden(
    div(
      id = paste0(tab, "_",type,"range"),
      fluidRow(
        column(6,
               numericInput(paste0(tab, "_",type,"min"), paste0("min(",type,")"),
                            value = NULL,
                            width = "100%")),
        column(6,
               numericInput(paste0(tab, "_",type,"max"), paste0("max(",type,")"),
                            value = NULL,
                            width = "100%"))),
    ))
}


# get_main_title ---------------------------------------------------------
##' @name get_main_title
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
##' 
get_main_title <- function(x, this.var, this.soil, this.air,
                           this.species, this.veg, this.leaf) {
  soil_var <- var_with_dim(x[[1]],"nsoil")
  air_var <- var_with_dim(x[[1]],"nair")
  species_var <- var_with_dim(x[[1]],"nspecies")
  veg_var <- var_with_dim(x[[1]],"nveg")
  leafage_var <- var_with_dim(x[[1]],"nleafage")
  
  tmp.title <- this.var
  if (this.var %in% soil_var) {
    soil.level <- get_soil_level(x[[1]], this.soil)
    min.soil <- round(soil.level[1]*1000, digits = 1)
    max.soil <- round(soil.level[2]*1000, digits = 1)
    tmp.title <- paste0(tmp.title, " ; nsoil = ", this.soil, " (",min.soil,"-",max.soil,"mm)")
  }
  if (this.var %in% air_var) {
    air.level <- get_air_level(x[[1]], this.air)
    min.air <- round(air.level[1], digits = 1)
    max.air <- round(air.level[2], digits = 1)
    tmp.title <- paste0(tmp.title, " ; nair = ", this.air, " (",min.air,"-",max.air,"m)")
  }
  if (this.var %in% species_var) {
    tmp.title <- paste0(tmp.title, " ; species = ", this.species)
  }
  if (this.var %in% veg_var) {
    air.level <- get_air_level(x[[1]], this.veg)
    min.air <- round(air.level[1], digits = 1)
    max.air <- round(air.level[2], digits = 1)
    tmp.title <- paste0(tmp.title, " ; nveg = ", this.veg, " (",min.air,"-",max.air,"m)")
  }
  if (this.var %in% leafage_var) {
    tmp.title <- paste0(tmp.title, " ; leaf age = ", this.leaf)
  }
  tmp.title
  
}

# get_shiny_filename ---------------------------------------------------------
##' @name get_shiny_filename
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
##' 
get_shiny_filename <- function(var, prefix, suffix, file.format) {
  basename <- var
  if (prefix != "") {
    basename <-   paste0(prefix, "_", basename)
  } 
  if (suffix != "") {
    basename <-   paste0(basename, "_", suffix)
  } 
  paste0(basename,".",file.format)
}

# add_changedate_button ---------------------------------------------------------
##' @name add_changedate_button
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
##'

add_changedate_button <- function(id, legend, class)
{
  fluidRow(class = class,
           actionButton(paste0(id, "_left"),
                        icon("minus"),
                        style = 'padding:4px; font-size:70%'),
           legend,
           actionButton(paste0(id, "_right"),
                        icon("plus"),
                        style = 'padding:4px; font-size:70%'))
}