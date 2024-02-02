# get_dynamic_input ---------------------------------------------------------
##' @name get_dynamic_input
##' @author Remi Lemaire-Patin
##' 
##' @title Generate variable selectInput
##' 
##' @description The function generates custom \code{selectInput}, used to
##'   subset variables in the shinymusica app. The \code{selectInput} is
##'   displayed only if the selected variable (\code{input.var}) matches the
##'   selected dimension (\code{this.var}).
##' 
##' 
##' @inheritParams shinymusica
##' @param this.var a \code{character}, dimension variable for which to generate 
##' the  \code{selectInput}. Can be either \code{nsoil}, \code{nair}, 
##' \code{nveg}, \code{nspecies} or \code{nleafage}.
##' @param input.name a \code{character}, reference name of the
##'   \code{selectInput}, used internally in the shiny app to refer to the
##'   results of the input form.
##' @param input.var a \code{character}, current variable selected in the app 
##' for which the \code{selectInput} is generated.
##' @param multiple (\emph{optional}, default \code{FALSE}), a \code{boolean},
##'   whether multiple choices are allowed
##' @param hide (\emph{optional}, default \code{FALSE}), a \code{boolean}, if 
##' \code{TRUE}, the \code{selectInput} is not displayed.
##' @return
##' 
##' A \code{selectInput} object
##' 
##' @family shiny
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' x.list <- list("model1" = x, "model2" = x)
##' # return selectInput for nsoil
##' get_dynamic_input(x.list, this.var = "nsoil", input.name = "tab2_nsoil", input.var = "w_soil")
##' # return NULL as nsoil is not a dimension for Qle
##' get_dynamic_input(x.list, this.var = "nsoil", input.name = "tab2_nsoil", input.var = "Qle")
##' @export
##' 
get_dynamic_input <- function(x, this.var, input.name, input.var,
                              multiple = FALSE, hide = FALSE) {
  
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
##' @title Generate variable selectInput for subsetting
##' 
##' @description The function generates custom \code{selectInput}, used to
##'   subset variables in the shinymusica app.
##' 
##' @inheritParams shinymusica
##' @param this.var a \code{character}, dimension variable for which to generate 
##' the  \code{selectInput}. Can be either \code{nsoil}, \code{nair}, 
##' \code{nveg}, \code{nspecies} or \code{nleafage}.
##' @param input.name a \code{character}, reference name of the
##'   \code{selectInput}, used internally in the shiny app to refer to the
##'   results of the input form.
##' @param multiple (\emph{optional}, default \code{FALSE}), a \code{boolean},
##'   whether multiple choices are allowed
##' @return
##' 
##' A \code{selectInput} object
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
##' @title hide or show a subset input 
##' 
##' @description This function toggle the state of some of the
##'  \code{selectInput} depending on the server state. It hides the one not 
##'  needed (which variable is not along the current variable dimension) and 
##'  show the one needed.
##' 
##' 
##' @param session, current shiny session
##' @param input a named \code{list} with UI input
##' @param index.var a \code{numeric}, from 1 to 3 to identify which of the 
##' app variable is concerned here
##' @param tab a \code{numeric}, 1 or 2 to identify which of the app tab is 
##' concerned here
##' @param x a named list of \code{ncdf4} object. Outputs generated by several
##'  MuSICA model run.
##' @param hide (\emph{optional}, default \code{FALSE}), a \code{boolean}, if 
##' \code{TRUE}, hides all \code{selectInput}
##' 
##' @return
##' 
##' NULL
##' 
##' @family shiny
##'#  x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##'#  x.list <- list("model1" = x, "model2" = x)
##'#  toggle_subset_input(session, input, index.var = 1, tab = 2, x = x.list, hide = FALSE)
##'
##' @examples
##' library(ncdf4)
##' 
##' 
##' @export
##' @importFrom shiny updateSelectInput

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
##' @title Generate \code{numericInput} to adjust ranges
##' 
##' @description This function returns two numeric input presented in a 
##' \code{fluidRow}. It assigns \code{ID} depending on the given \code{tab} and
##' the aesthetic concerned (\code{type}).
##' 
##' 
##' @param tab a \code{numeric}, 1 or 2 to identify which of the app tab is 
##' concerned here.
##' @param type a \code{character}, aesthetics for which \code{numericInput} 
##' should be displayed. Can be \code{'x'}, \code{'y'} or \code{'fill'}.
##' @return
##' 
##' A set of \code{numericInput} object
##' 
##' @family shiny
##'   
##' @examples
##' get_range_input(1, "fill")
##' get_range_input(2, "x")
##' get_range_input(2, "y")
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
##' @title generate proper title for dygraph
##' 
##' @description This function generate a detailed title for the dygraph in 
##' the first tab of the shiny app. The title include the potential subsetting
##' of dimension and the depth/height associated to the layer represented in
##' the plot.
##' 
##' @inheritParams shinymusica
##' @param this.var a \code{character}, currently selected variable
##' @param this.soil a \code{numeric}, currently selected \code{nsoil}
##' @param this.air a \code{numeric}, currently selected \code{nair}
##' @param this.species a \code{numeric}, currently selected \code{nspecies}
##' @param this.veg a \code{numeric}, currently selected \code{nveg}
##' @param this.leaf a \code{numeric}, currently selected \code{nleafage}
##' 
##' @return
##' 
##' A \code{ggplot} object
##' 
##' @family shiny
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' x.list <- list("model1" = x, "model2" = x)
##' get_main_title(x.list, this.var = "w_soil", this.soil = 12,
##'                        this.air = NULL, this.species = NULL, 
##'                        this.veg = NULL, this.leaf = NULL)
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
##' @title Generate filename for export
##' 
##' @description This function generate a filename used as a suggestion to
##'   export and download the graphics from tab2. The filename is based around
##'   the current variable (\code{var}), a \code{prefix}, a \code{suffix} and
##'   the selected \code{file.format}.
##' 
##' 
##' @param var a \code{character}, currently selected variable
##' @param prefix a \code{character}, prefix to add to the filename
##' @param suffix a \code{character}, suffix to add to the filename
##' @param file.format a \code{character}, file format
##' 
##' @return
##' 
##' A \code{character}
##' 
##' @family shiny
##'   
##' @examples
##' get_shiny_filename(var = "Qle", prefix = "LeBray", suffix = "crack_depth_1", file.format = "png")
##' get_shiny_filename(var = "Qle", prefix = "", suffix = "", file.format = "png")
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
##' @title Generate +/- button to adjust dates
##' 
##' @description This function generates two \code{actionButton} organized 
##' within a \code{HTML} \code{fluidRow}. The buttons can then be used within 
##' the app to adjust the date window.
##' 
##' 
##' @param id a \code{character}, used to generate the \code{actionButton} 
##' \code{id}.
##' @param legend a \code{character}, separate the two  \code{actionButton} 
##' @param class a \code{character}, define the \code{HTML class} used by the
##'   \code{fluidRow}. This class is then used to format the \code{actionButton}
##' @return
##' 
##' \code{HTML} code withe two \code{actionButton} 
##' 
##' @family shiny
##'   
##' @examples
##' add_changedate_button(id = "datemin", legend = "Month", class = "dateclass")
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
                        class = "changedate_button"),
           legend,
           actionButton(paste0(id, "_right"),
                        icon("plus"),
                        class = "changedate_button"))
}