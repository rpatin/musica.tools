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
##' @importFrom shiny shinyServer renderUI selectInput eventReactive reactive
##' @importFrom dygraphs renderDygraph
##' @importFrom htmltools div
##' @importFrom lubridate `year<-` year `day<-` day `month<-` month
##' @importFrom cowplot save_plot
##' @export
##' 
##' 

musica_server <- function(x) {
  server <- shinyServer(function(input, output, session) {
    soil_var <- var_with_dim(x[[1]],"nsoil")
    air_var <- var_with_dim(x[[1]],"nair")
    species_var <- var_with_dim(x[[1]],"nspecies")
    veg_var <- var_with_dim(x[[1]],"nveg")
    leafage_var <- var_with_dim(x[[1]],"nleafage")
    
    # Tab1 ---------------------------------------------------------------
    ## input Tab1 ------------------------------------------------------------
    
    
    ### time Tab1 ------------------------------------------------------------
    
    observeEvent(input$tab1_datemax_left, {
      updateSliderInput(session, "tab1_time_range",
                        value = input$tab1_time_range - 24*3600*c(0,1),
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemax_right, {
      updateSliderInput(session, "tab1_time_range",
                        value = input$tab1_time_range + 24*3600*c(0,1),
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemin_left, {
      updateSliderInput(session, "tab1_time_range", 
                        value = input$tab1_time_range - 24*3600*c(1,0),
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemin_right, {
      updateSliderInput(session, "tab1_time_range", 
                        value = input$tab1_time_range + 24*3600*c(1,0),
                        timeFormat = "%F")
    })    
    observeEvent(input$tab1_time_range, {
      updateSliderInput(session, "tab2_timerange",
                        value = input$tab1_time_range)
    })
    ### soil Tab1 ------------------------------------------------------------
    
    
    output$tab1_dynamic_nsoil1 <-
      renderUI(
        reactive({
          function(this.input){ get_dynamic_input(x, "nsoil", "tab1_nsoil1", this.input)}
        })()(input$tab1_var1))
    
    output$tab1_dynamic_nsoil2 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nsoil", "tab1_nsoil2", this.input)}
        })()(input$tab1_var2))
    
    output$tab1_dynamic_nsoil3 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nsoil", "tab1_nsoil3", this.input)}
        })()(input$tab1_var3))
    
    ### air Tab1 -------------------------------------------------------------
    
    output$tab1_dynamic_nair1 <-
      renderUI(
        reactive({
          function(this.input){ get_dynamic_input(x, "nair", "tab1_nair1", this.input)}
        })()(input$tab1_var1))
    
    output$tab1_dynamic_nair2 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nair", "tab1_nair2", this.input)}
        })()(input$tab1_var2))
    
    output$tab1_dynamic_nair3 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nair", "tab1_nair3", this.input)}
        })()(input$tab1_var3))
    
    
    
    ### nspecies Tab1 --------------------------------------------------------
    
    output$tab1_dynamic_nspecies1 <-
      renderUI(
        reactive({
          function(this.input){ get_dynamic_input(x, "nspecies", "tab1_nspecies1", this.input)}
        })()(input$tab1_var1))
    
    output$tab1_dynamic_nspecies2 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nspecies", "tab1_nspecies2", this.input)}
        })()(input$tab1_var2))
    
    output$tab1_dynamic_nspecies3 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nspecies", "tab1_nspecies3", this.input)}
        })()(input$tab1_var3))
    
    
    
    
    ### nveg Tab1 -----------------------------------------------------------
    
    
    output$tab1_dynamic_nveg1 <-
      renderUI(
        reactive({
          function(this.input){ get_dynamic_input(x, "nveg", "tab1_nveg1", this.input)}
        })()(input$tab1_var1))
    
    output$tab1_dynamic_nveg2 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nveg", "tab1_nveg2", this.input)}
        })()(input$tab1_var2))
    
    output$tab1_dynamic_nveg3 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nveg", "tab1_nveg3", this.input)}
        })()(input$tab1_var3))
    
    ### nleafage Tab1 ------------------------------------------------------
    
    
    output$tab1_dynamic_nleafage1 <-
      renderUI(
        reactive({
          function(this.input){ get_dynamic_input(x, "nleafage", "tab1_nleafage1", this.input)}
        })()(input$tab1_var1))
    
    output$tab1_dynamic_nleafage2 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nleafage", "tab1_nleafage2", this.input)}
        })()(input$tab1_var2))
    
    output$tab1_dynamic_nleafage3 <-
      renderUI(
        reactive({
          function(this.input){get_dynamic_input(x, "nleafage", "tab1_nleafage3", this.input)}
        })()(input$tab1_var3))
    
    ## reactive title Tab1 ---------------------------------------------------
    
    tab1_main.title1 <- eventReactive(input$tab1_UpdateView,{
      get_main_title(x,
                     this.var = input$tab1_var1,
                     this.soil = input$tab1_nsoil1,
                     this.air = input$tab1_nair1,
                     this.species = input$tab1_nspecies1,
                     this.veg = input$tab1_nveg1,
                     this.leaf = input$tab1_nleafage1)
    })
    
    tab1_main.title2 <- eventReactive(input$tab1_UpdateView,{
      get_main_title(x,
                     this.var = input$tab1_var2,
                     this.soil = input$tab1_nsoil2,
                     this.air = input$tab1_nair2,
                     this.species = input$tab1_nspecies2,
                     this.veg = input$tab1_nveg2,
                     this.leaf = input$tab1_nleafage2)
    })
    
    tab1_main.title3 <- eventReactive(input$tab1_UpdateView,{
      get_main_title(x,
                     this.var = input$tab1_var3,
                     this.soil = input$tab1_nsoil3,
                     this.air = input$tab1_nair3,
                     this.species = input$tab1_nspecies3,
                     this.veg = input$tab1_nveg3,
                     this.leaf = input$tab1_nleafage3)
    })
    ## reactive data Tab1 ---------------------------------------------------
    
    
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
    
    
    ## output Tab1 -------------------------------------------------------
    
    
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
    
    # Tab 2 ---------------------------------------------------------------
    ## input Tab2 ------------------------------------------------------------
    
    ### DateTime Tab2 ------------------------------------------------------
    observeEvent(input$tab2_date_left, {
      updateSliderInput(session, "tab2_timerange",
                        value = input$tab2_timerange - 7*24*3600*c(1,1))
    })
    observeEvent(input$tab2_date_right, {
      updateSliderInput(session, "tab2_timerange",
                        value = input$tab2_timerange + 7*24*3600*c(1,1))
    })
    observeEvent(input$tab2_datemax_left, {
      updateSliderInput(session, "tab2_timerange",
                        value = input$tab2_timerange - 24*3600*c(0,1))
    })
    observeEvent(input$tab2_datemax_right, {
      updateSliderInput(session, "tab2_timerange", 
                        value = input$tab2_timerange + 24*3600*c(0,1))
    })
    observeEvent(input$tab2_datemin_left, {
      updateSliderInput(session, "tab2_timerange",
                        value = input$tab2_timerange - 24*3600*c(1,0))
    })
    observeEvent(input$tab2_datemin_right, {
      updateSliderInput(session, "tab2_timerange",
                        value = input$tab2_timerange + 24*3600*c(1,0))
    })
    
    observeEvent(input$tab2_timemax_left, {
      updateSliderInput(session, "tab2_timerange",
                        value = input$tab2_timerange - 1800*c(0,1))
    })
    observeEvent(input$tab2_timemax_right, {
      updateSliderInput(session, "tab2_timerange",
                        value = input$tab2_timerange + 1800*c(0,1))
    })
    observeEvent(input$tab2_timemin_left, {
      updateSliderInput(session, "tab2_timerange", 
                        value = input$tab2_timerange - 1800*c(1,0))
    })
    observeEvent(input$tab2_timemin_right, {
      updateSliderInput(session, "tab2_timerange", 
                        value = input$tab2_timerange + 1800*c(1,0))
    })
    
    
    ### Selected Models -------------------------------------------------------
    observeEvent(input$tab1_selected_output, {
      updateSelectInput(session, "tab2_selected_output",
                        selected = input$tab1_selected_output)
    })
    
    ### Selected Variables ----------------------------------------------------
    observeEvent(input$tab1_var1, {
      updateSelectInput(session, "tab2_var",
                        selected = input$tab1_var1)
    })
    
    
    
    ### Reset levels --------------------------------------------------------
    
    observeEvent(input$tab2_subset, {
      if (input$tab2_subset) {
        updateSelectInput(session, "tab2_nsoil",
                          selected = input$tab1_nsoil1)
        updateSelectInput(session, "tab2_nair",
                          selected = input$tab1_nair1)
        updateSelectInput(session, "tab2_nveg",
                          selected = input$tab1_nveg1)
        updateSelectInput(session, "tab2_nspecies",
                          selected = input$tab1_nspecies1)
        updateSelectInput(session, "tab2_nleafage",
                          selected = input$tab1_nleafage1)
      } 
    })
    
    observeEvent(input$tab2_type, {
      if (input$tab2_type == "heatmap") {
        updateCheckboxInput(session, "tab2_subset",
                            value = FALSE)
      }
    })
    ### Reactive dim input -------------------------------------------------
    
    
    output$tab2_dynamic_date <-
      renderUI({
        eventReactive(input$tab1_time_range, {
          sliderInput("tab2_timerange", label = "Date",
                      min = get_time_range(x)[1],
                      max = get_time_range(x)[2],
                      value = input$tab1_time_range,
                      timeFormat = "%F %T",
                      step = 24*3600,
                      round = 4) 
        })()})
    
    output$tab2_dynamic_nsoil <-
      renderUI(
        reactive({
          function(this.input){ 
            get_dynamic_input(x, "nsoil", "tab2_nsoil", this.input,
                              multiple = TRUE, hide = !input$tab2_subset)
          }
        })()(input$tab2_var))
    
    output$tab2_dynamic_nair <-
      renderUI(
        reactive({
          function(this.input){ 
            get_dynamic_input(x, "nair", "tab2_nair", this.input,
                              multiple = TRUE, hide = !input$tab2_subset)
          }
        })()(input$tab2_var))
    
    output$tab2_dynamic_nspecies <-
      renderUI(
        reactive({
          function(this.input){ 
            get_dynamic_input(x, "nspecies", "tab2_nspecies", this.input,
                              multiple = TRUE, hide = !input$tab2_subset)
          }
          
        })()(input$tab2_var))
    
    output$tab2_dynamic_nveg <-
      renderUI(
        reactive({
          function(this.input){ 
            get_dynamic_input(x, "nveg", "tab2_nveg", this.input,
                              multiple = TRUE, hide = !input$tab2_subset)
          }
        })()(input$tab2_var))
    
    output$tab2_dynamic_nleafage <-
      renderUI(
        reactive({
          function(this.input){ 
            get_dynamic_input(x, "nleafage", "tab2_nleafage", this.input,
                              multiple = TRUE, hide = !input$tab2_subset)
          }
        })()(input$tab2_var))
    
    ## data and plot generation -----------------------------------------------
    
    tab2_df <- eventReactive(input$tab2_UpdateView, {
      if (input$tab2_subset) {
        df <- 
          get_variable_comparison(x[input$tab2_selected_output],
                                  this_var = input$tab2_var,
                                  time_range = input$tab2_timerange,
                                  list.soil.level = input$tab2_nsoil,
                                  list.air.level = input$tab2_nair,
                                  list.species.level = input$tab2_nspecies,
                                  list.veg.level = input$tab2_nveg,
                                  list.leafage.level = input$tab2_nleafage)
      } else {
        df <- 
          get_variable_comparison(x[input$tab2_selected_output],
                                  this_var = input$tab2_var,
                                  time_range = input$tab2_timerange)
      }
      df
    })
    
    tab2_plot <- eventReactive(input$tab2_UpdateView, {
      ggplot_variable(tab2_df(),
                      out.type = input$tab2_type,
                      color = input$tab2_color,
                      linetype = input$tab2_linetype,
                      facet_formula = input$tab2_facet)
    })
    output$tab2_plot <- 
      renderPlot(tab2_plot())
    
    output$tab2_download <- downloadHandler(
      filename = function() { paste(input$dataset, '.png', sep = '') },
      content = function(file) {
        save_plot(file, tab2_plot(), base_width = 30/cm(1), base_height = 20/cm(1))
      }
    )
  }) #end shinyserver
} # end