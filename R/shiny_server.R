# musica_server ---------------------------------------------------------
##' @name musica_server
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
##' @importFrom shiny shinyServer renderUI selectInput eventReactive
##' @importFrom dygraphs renderDygraph
##' @importFrom htmltools div
##' @export
##' 
##' 

musica_server <- function(x) {
  server <- shinyServer(function(input, output) {
    soil_var <- var_with_dim(x[[1]],"nsoil")
    air_var <- var_with_dim(x[[1]],"nair")
    species_var <- var_with_dim(x[[1]],"nspecies")
    veg_var <- var_with_dim(x[[1]],"nveg")
    leafage_var <- var_with_dim(x[[1]],"nleafage")
    
    # input -------------------------------------------------------------------
    
    ## soil --------------------------------------------------------------------
    
    output$tab1_dynamic_nsoil1 <- renderUI({
      if (input$tab1_var1 %in% soil_var) {
        selectInput("tab1_nsoil1", label = div(style = "font-size:10px", "Soil layer"),
                    choices = get_dim_value(x[[1]], "nsoil"),
                    selected = 1,
                    width = "50%")
      } else {
        return(NULL)
      }
    })
    
    output$tab1_dynamic_nsoil2 <- renderUI({
      if (input$tab1_var2 %in% soil_var) {
        selectInput("tab1_nsoil2", label = div(style = "font-size:10px", "Soil layer"),
                    choices = get_dim_value(x[[1]], "nsoil"),
                    selected = 1,
                    width = "50%")
      } else {
        return(NULL)
      }
    })
    output$tab1_dynamic_nsoil3 <- renderUI({
      if (input$tab1_var3 %in% soil_var) {
        selectInput("tab1_nsoil3", label = div(style = "font-size:10px", "Soil layer"),
                    choices = get_dim_value(x[[1]], "nsoil"),
                    selected = 1,
                    width = "50%")
      } else {
        return(NULL)
      }
    })
    
    ## air --------------------------------------------------------------------
    
    output$tab1_dynamic_nair1 <- renderUI({
      if (input$tab1_var1 %in% air_var) {
        selectInput("tab1_nair1", label = div(style = "font-size:10px", "Air layer"),
                    choices = get_dim_value(x[[1]], "nair"),
                    selected = 1,
                    width = "50%")
      } else {
        return(NULL)
      }
    })
    
    output$tab1_dynamic_nair2 <- renderUI({
      if (input$tab1_var2 %in% air_var) {
        selectInput("tab1_nair2", label = div(style = "font-size:10px", "Air layer"),
                    choices = get_dim_value(x[[1]], "nair"),
                    selected = 1,
                    width = "50%")
      } else {
        return(NULL)
      }
    })
    output$tab1_dynamic_nair3 <- renderUI({
      if (input$tab1_var3 %in% air_var) {
        selectInput("tab1_nair3", label = div(style = "font-size:10px", "Air layer"),
                    choices = get_dim_value(x[[1]], "nair"),
                    selected = 1,
                    width = "50%")
      } else {
        return(NULL)
      }
    })
    
    
    ## nspecies --------------------------------------------------------------------
    
    output$tab1_dynamic_nspecies1 <- renderUI({
      if (input$tab1_var1 %in% species_var) {
        selectInput("tab1_nspecies1", label = div(style = "font-size:10px", "Species"),
                    choices = get_dim_value(x[[1]], "nspecies"),
                    selected = 1,
                    width = "100%")
      } else {
        return(NULL)
      }
    })
    
    output$tab1_dynamic_nspecies2 <- renderUI({
      if (input$tab1_var2 %in% species_var) {
        selectInput("tab1_nspecies2", label = div(style = "font-size:10px", "Species"),
                    choices = get_dim_value(x[[1]], "nspecies"),
                    selected = 1,
                    width = "100%")
      } else {
        return(NULL)
      }
    })
    
    output$tab1_dynamic_nspecies3 <- renderUI({
      if (input$tab1_var3 %in% species_var) {
        selectInput("tab1_nspecies3", label = div(style = "font-size:10px", "Species"),
                    choices = get_dim_value(x[[1]], "nspecies"),
                    selected = 1,
                    width = "100%")
      } else {
        return(NULL)
      }
    })
    
    ## nveg --------------------------------------------------------------------
    
    output$tab1_dynamic_nveg1 <- renderUI({
      if (input$tab1_var1 %in% veg_var) {
        selectInput("tab1_nveg1", label = div(style = "font-size:10px", "Vegetation Layer"),
                    choices = get_dim_value(x[[1]], "nveg"),
                    selected = 1,
                    width = "100%")
      } else {
        return(NULL)
      }
    })
    
    output$tab1_dynamic_nveg2 <- renderUI({
      if (input$tab1_var2 %in% veg_var) {
        selectInput("tab1_nveg2", label = div(style = "font-size:10px", "Vegetation Layer"),
                    choices = get_dim_value(x[[1]], "nveg"),
                    selected = 1,
                    width = "100%")
      } else {
        return(NULL)
      }
    })
    
    output$tab1_dynamic_nveg3 <- renderUI({
      if (input$tab1_var3 %in% veg_var) {
        selectInput("tab1_nveg3", label = div(style = "font-size:10px", "Vegetation Layer"),
                    choices = get_dim_value(x[[1]], "nveg"),
                    selected = 1,
                    width = "100%")
      } else {
        return(NULL)
      }
    })
    
    ## nleafage --------------------------------------------------------------------
    
    output$tab1_dynamic_nleafage1 <- renderUI({
      if (input$tab1_var1 %in% leafage_var) {
        selectInput("tab1_nleafage1", label = div(style = "font-size:10px", "Leaf Age"),
                    choices = get_dim_value(x[[1]], "nleafage"),
                    selected = 1,
                    width = "100%")
      } else {
        return(NULL)
      }
    })
    
    output$tab1_dynamic_nleafage2 <- renderUI({
      if (input$tab1_var2 %in% leafage_var) {
        selectInput("tab1_nleafage2", label = div(style = "font-size:10px", "Leaf Age"),
                    choices = get_dim_value(x[[1]], "nleafage"),
                    selected = 1,
                    width = "100%")
      } else {
        return(NULL)
      }
    })
    
    output$tab1_dynamic_nleafage3 <- renderUI({
      if (input$tab1_var3 %in% leafage_var) {
        selectInput("tab1_nleafage3", label = div(style = "font-size:10px", "Leaf Age"),
                    choices = get_dim_value(x[[1]], "nleafage"),
                    selected = 1,
                    width = "100%")
      } else {
        return(NULL)
      }
    })
    # reactive title -----------------------------------------------------------
    
    tab1_main.title1 <- eventReactive(input$tab1_UpdateView,{
      tmp.title <- input$tab1_var1
      if (input$tab1_var1 %in% soil_var) {
        soil.level <- get_soil_level(x[[1]], input$tab1_nsoil1)
        min.soil <- round(soil.level[1], digits = 1)
        max.soil <- round(soil.level[2], digits = 1)
        tmp.title <- paste0(tmp.title, " ; nsoil = ", input$tab1_nsoil1, " (",min.soil,"-",max.soil,"cm)")
      }
      if (input$tab1_var1 %in% air_var) {
        air.level <- get_air_level(x[[1]], input$tab1_nair1)
        min.air <- round(air.level[1], digits = 1)
        max.air <- round(air.level[2], digits = 1)
        tmp.title <- paste0(tmp.title, " ; nair = ", input$tab1_nair1, " (",min.air,"-",max.air,"m)")
      }
      if (input$tab1_var1 %in% species_var) {
        tmp.title <- paste0(tmp.title, " ; species = ", input$tab1_nspecies1)
      }
      if (input$tab1_var1 %in% veg_var) {
        air.level <- get_air_level(x[[1]], input$tab1_nveg1)
        min.air <- round(air.level[1], digits = 1)
        max.air <- round(air.level[2], digits = 1)
        tmp.title <- paste0(tmp.title, " ; nveg = ", input$tab1_nair1, " (",min.air,"-",max.air,"m)")
      }
      tmp.title
    })
    
    tab1_main.title2 <- eventReactive(input$tab1_UpdateView,{
      tmp.title <- input$tab1_var2
      if (input$tab1_var2 %in% soil_var) {
        soil.level <- get_soil_level(x[[1]], input$tab1_nsoil2)
        min.soil <- round(soil.level[1], digits = 1)
        max.soil <- round(soil.level[2], digits = 1)
        tmp.title <- paste0(tmp.title, " ; nsoil = ", input$tab1_nsoil2, " (",min.soil,"-",max.soil,"cm)")
      }
      if (input$tab1_var2 %in% air_var) {
        air.level <- get_air_level(x[[1]], input$tab1_nair2)
        min.air <- round(air.level[1], digits = 1)
        max.air <- round(air.level[2], digits = 1)
        tmp.title <- paste0(tmp.title, " ; nair = ", input$tab1_nair2, " (",min.air,"-",max.air,"m)")
      }
      if (input$tab1_var2 %in% species_var) {
        tmp.title <- paste0(tmp.title, " ; species = ", input$tab1_nspecies2)
      }
      if (input$tab1_var2 %in% veg_var) {
        air.level <- get_air_level(x[[1]], input$tab1_nveg2)
        min.air <- round(air.level[1], digits = 1)
        max.air <- round(air.level[2], digits = 1)
        tmp.title <- paste0(tmp.title, " ; nveg = ", input$tab1_nveg2, " (",min.air,"-",max.air,"m)")
      }
      tmp.title
    })
    
    tab1_main.title3 <- eventReactive(input$tab1_UpdateView,{
      tmp.title <- input$tab1_var3
      if (input$tab1_var3 %in% soil_var) {
        soil.level <- get_soil_level(x[[1]], input$tab1_nsoil3)
        min.soil <- round(soil.level[1], digits = 1)
        max.soil <- round(soil.level[2], digits = 1)
        tmp.title <- paste0(tmp.title, " ; nsoil = ", input$tab1_nsoil3, " (",min.soil,"-",max.soil,"cm)")
      }
      if (input$tab1_var3 %in% air_var) {
        air.level <- get_air_level(x[[1]], input$tab1_nair3)
        min.air <- round(air.level[1], digits = 1)
        max.air <- round(air.level[2], digits = 1)
        tmp.title <- paste0(tmp.title, " ; nair = ", input$tab1_nair3, " (",min.air,"-",max.air,"m)")
      }
      if (input$tab1_var3 %in% species_var) {
        tmp.title <- paste0(tmp.title, " ; species = ", input$tab1_nspecies3)
      }
      if (input$tab1_var3 %in% veg_var) {
        air.level <- get_air_level(x[[1]], input$tab1_nveg3)
        min.air <- round(air.level[1], digits = 1)
        max.air <- round(air.level[2], digits = 1)
        tmp.title <- paste0(tmp.title, " ; nveg = ", input$tab1_nveg3, " (",min.air,"-",max.air,"m)")
      }
      tmp.title
    })
    # reactive data ------------------------------------------------------------
    
    
    tab1_df1 <- eventReactive(input$tab1_UpdateView, {
      get_variable_comparison(x[input$tab1_selected_output],input$tab1_var1,
                              time_range = input$tab1_time_range,
                              list.soil.level = input$tab1_nsoil1,
                              list.air.level = input$tab1_nair1,
                              list.species.level = input$tab1_nspecies1,
                              list.veg.level = input$tab1_nveg1,
                              list.leafage.level = input$tab1_nleafage1)
    })
    tab1_df2 <- eventReactive(input$tab1_UpdateView, {
      get_variable_comparison(x[input$tab1_selected_output],input$tab1_var2,
                              time_range = input$tab1_time_range,
                              list.soil.level = input$tab1_nsoil2,
                              list.air.level = input$tab1_nair2,
                              list.species.level = input$tab1_nspecies2,
                              list.veg.level = input$tab1_nveg2,
                              list.leafage.level = input$tab1_nleafage2)
    })
    tab1_df3 <- eventReactive(input$tab1_UpdateView, {
      get_variable_comparison(x[input$tab1_selected_output],input$tab1_var3,
                              time_range = input$tab1_time_range,
                              list.soil.level = input$tab1_nsoil3,
                              list.air.level = input$tab1_nair3,
                              list.species.level = input$tab1_nspecies3,
                              list.veg.level = input$tab1_nveg3,
                              list.leafage.level = input$tab1_nleafage3)
    })
    
    
    # output ------------------------------------------------------------------
    
    
    output$tab1_dygraph1 <- renderDygraph({
      dygraph_comparison(tab1_df1(),
                         main.title = tab1_main.title1(),
                         pixwidth = 600, 
                         pixheight = 40)
    })
    output$tab1_dygraph2 <- renderDygraph({
      dygraph_comparison(tab1_df2(),
                         main.title = tab1_main.title2(),
                         pixwidth = 600, 
                         pixheight = 40)
    })
    output$tab1_dygraph3 <- renderDygraph({
      dygraph_comparison(tab1_df3(),
                         main.title = tab1_main.title3(),
                         pixwidth = 600, 
                         pixheight = 40)
    })
  })
}