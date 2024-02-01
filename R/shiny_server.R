# musica_server ---------------------------------------------------------
##' @name musica_server
##' @author Remi Lemaire-Patin
##' 
##' @title Shiny server for \code{\link{shinymusica}}
##' 
##' @description This function generates a shiny server for \code{\link{shinymusica}}
##' 
##' @inheritParams shinymusica
##' 
##' @return
##' 
##' A \code{shiny server} object
##' 
##' @family shiny
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' x.list <- list("model1" = x, "model2" = x)
##' x.server <- musica_server(x.list)
##' 
##' @importFrom shiny shinyServer renderUI selectInput eventReactive reactive
##' observeEvent updateSliderInput updateSelectInput updateSelectizeInput 
##' updateTextInput renderPlot downloadHandler
##' @importFrom shinyjs reset show hide toggle
##' @importFrom dygraphs renderDygraph
##' @importFrom htmltools div
##' @importFrom lubridate year<- year day<- day month<- month
##' @importFrom cowplot save_plot
##' @importFrom stringr str_subset
##' @importFrom grDevices cm
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
    
    #### date window Tab1 ----------------------------------------------------
    
    ##### year --------------------------------------------------------#
    observeEvent(input$tab1_window_year_left, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin - 24*3600*365,
                        timeFormat = "%F")
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax - 24*3600*365,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_window_year_right, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin + 24*3600*365,
                        timeFormat = "%F")
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax + 24*3600*365,
                        timeFormat = "%F")
    })   
    ##### month --------------------------------------------------------#
    observeEvent(input$tab1_window_month_left, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin - 24*3600*30,
                        timeFormat = "%F")
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax - 24*3600*30,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_window_month_right, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin + 24*3600*30,
                        timeFormat = "%F")
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax + 24*3600*30,
                        timeFormat = "%F")
    })   
    ##### week -------------------------------------------------------#
    observeEvent(input$tab1_window_week_left, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin - 24*3600*7,
                        timeFormat = "%F")
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax - 24*3600*7,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_window_week_right, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin + 24*3600*7,
                        timeFormat = "%F")
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax + 24*3600*7,
                        timeFormat = "%F")
    })    
    ##### day --------------------------------------------------------#
    observeEvent(input$tab1_window_day_left, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin - 24*3600,
                        timeFormat = "%F")
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax - 24*3600,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_window_day_right, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin + 24*3600,
                        timeFormat = "%F")
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax + 24*3600,
                        timeFormat = "%F")
    })   
    
    #### datemin Tab1 --------------------------------------------------------
    
    ##### year --------------------------------------------------------#
    observeEvent(input$tab1_datemin_year_left, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin - 24*3600*365,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemin_year_right, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin + 24*3600*365,
                        timeFormat = "%F")
    })   
    ##### month --------------------------------------------------------#
    observeEvent(input$tab1_datemin_month_left, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin - 24*3600*30,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemin_month_right, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin + 24*3600*30,
                        timeFormat = "%F")
    })   
    ##### week -------------------------------------------------------#
    observeEvent(input$tab1_datemin_week_left, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin - 24*3600*7,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemin_week_right, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin + 24*3600*7,
                        timeFormat = "%F")
    })    
    ##### day --------------------------------------------------------#
    observeEvent(input$tab1_datemin_day_left, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin - 24*3600,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemin_day_right, {
      updateSliderInput(session, "tab1_datemin", 
                        value = input$tab1_datemin + 24*3600,
                        timeFormat = "%F")
    })    
    ##### tab2_datemin --------------------------------------------------------#
    observeEvent(input$tab1_datemin, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab1_datemin)
    })
    
    
    #### datemax Tab1 ---------------------------------------------------------
    ##### year --------------------------------------------------------#
    observeEvent(input$tab1_datemax_year_left, {
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax - 24*3600*365,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemax_year_right, {
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax + 24*3600*365,
                        timeFormat = "%F")
    })    
    ##### month --------------------------------------------------------#
    observeEvent(input$tab1_datemax_month_left, {
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax - 24*3600*30,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemax_month_right, {
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax + 24*3600*30,
                        timeFormat = "%F")
    })    
    ##### week --------------------------------------------------------#
    observeEvent(input$tab1_datemax_week_left, {
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax - 24*3600*7,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemax_week_right, {
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax + 24*3600*7,
                        timeFormat = "%F")
    })    
    ##### day --------------------------------------------------------#
    observeEvent(input$tab1_datemax_day_left, {
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax - 24*3600,
                        timeFormat = "%F")
    })
    observeEvent(input$tab1_datemax_day_right, {
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax + 24*3600,
                        timeFormat = "%F")
    })    
    ##### tab2_datemax --------------------------------------------------------#
    observeEvent(input$tab1_datemax, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab1_datemax)
    })
    
    ### Toggle diffmodels -----------------------------------------
    observeEvent({
      input$tab1_selected_output
    }, {
      if (length(input$tab1_selected_output) > 1)  {
        shinyjs::show("tab1_diffmodels1")
        shinyjs::show("tab1_diffmodels2")
        shinyjs::show("tab1_diffmodels3")
      } else {
        reset("tab1_diffmodels1")
        reset("tab1_diffmodels2")
        reset("tab1_diffmodels3")
        hide("tab1_diffmodels1")
        hide("tab1_diffmodels2")
        hide("tab1_diffmodels3")
      }
    })
    ### Subset Tab1 ------------------------------------------------------------
    
    observeEvent({
      input$tab1_var1
    }, {
      toggle_subset_input(session, input, index.var = 1, x,
                          tab = "tab1")
    })
    
    observeEvent({
      input$tab1_var2
    }, {
      toggle_subset_input(session, input, index.var = 2, x,
                          tab = "tab1")
    })
    
    observeEvent({
      input$tab1_var3
    }, {
      toggle_subset_input(session, input, index.var = 3, x,
                          tab = "tab1")
    })
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
    
    ### tab1_df1 ---------------------------------------------------
    
    tab1_df1 <- eventReactive({
      input$tab1_UpdateView
    }, {
      if(input$tab1_plotvar1) {
        return(
          get_variable_comparison(x = x[input$tab1_selected_output],
                                  varname = input$tab1_var1,
                                  time_range = c(input$tab1_datemin, input$tab1_datemax),
                                  list.soil.level = input$tab1_nsoil1,
                                  list.air.level = input$tab1_nair1,
                                  list.species.level = input$tab1_nspecies1,
                                  list.veg.level = input$tab1_nveg1,
                                  list.leafage.level = input$tab1_nleafage1,
                                  diffmodels = input$tab1_diffmodels1))
      } else {
        return(NULL)
      }
    })
    ### tab1_df2 ---------------------------------------------------
    tab1_df2 <- eventReactive({
      input$tab1_UpdateView
    }, {
      if(input$tab1_plotvar2) {
        return(
          get_variable_comparison(x[input$tab1_selected_output],input$tab1_var2,
                                  time_range = c(input$tab1_datemin, input$tab1_datemax),
                                  list.soil.level = input$tab1_nsoil2,
                                  list.air.level = input$tab1_nair2,
                                  list.species.level = input$tab1_nspecies2,
                                  list.veg.level = input$tab1_nveg2,
                                  list.leafage.level = input$tab1_nleafage2,
                                  diffmodels = input$tab1_diffmodels2))
      } else {
        return(NULL)
      }
    })
    ### tab1_df3 ---------------------------------------------------
    tab1_df3 <- eventReactive({
      input$tab1_UpdateView
    }, {
      if(input$tab1_plotvar3) {
        return(
          get_variable_comparison(x[input$tab1_selected_output],input$tab1_var3,
                                  time_range = c(input$tab1_datemin, input$tab1_datemax),
                                  list.soil.level = input$tab1_nsoil3,
                                  list.air.level = input$tab1_nair3,
                                  list.species.level = input$tab1_nspecies3,
                                  list.veg.level = input$tab1_nveg3,
                                  list.leafage.level = input$tab1_nleafage3,
                                  diffmodels = input$tab1_diffmodels3))
      } else {
        return(NULL)
      }
    })
    
    
    ## output Tab1 -------------------------------------------------------
    
    
    ### tab1_dygraph1 -------------------------------------------------------
    tab1_dygraph1 <- eventReactive({
      input$tab1_UpdateView
    }, {
      if (input$tab1_plotvar1) {
        return(
          dygraph_comparison(tab1_df1(),
                             main.title = tab1_main.title1(),
                             pixwidth = 600, 
                             pixheight = 40,
                             diffmodels = input$tab1_diffmodels1))
      } else {
        return(NULL)
      }
    })
    output$tab1_dygraph1 <- renderDygraph({tab1_dygraph1()})
    ### tab1_dygraph2 -------------------------------------------------------
    tab1_dygraph2 <- eventReactive({
      input$tab1_UpdateView
    }, {
      if (input$tab1_plotvar2) {
        return(
          dygraph_comparison(tab1_df2(),
                             main.title = tab1_main.title2(),
                             pixwidth = 600, 
                             pixheight = 40,
                             diffmodels = input$tab1_diffmodels2))
      } else {
        return(NULL)
      }
    })
    output$tab1_dygraph2 <- renderDygraph(tab1_dygraph2())
    ### tab1_dygraph3 -------------------------------------------------------
    tab1_dygraph3 <- eventReactive({
      input$tab1_UpdateView
    }, {
      if (input$tab1_plotvar3) {
        return(
          dygraph_comparison(tab1_df3(),
                             main.title = tab1_main.title3(),
                             pixwidth = 600, 
                             pixheight = 40,
                             diffmodels = input$tab1_diffmodels3))
      } else {
        return(NULL)
      }
    })
    output$tab1_dygraph3 <- renderDygraph(tab1_dygraph3())
    
    # Tab 2 ---------------------------------------------------------------
    ## input Tab2 ------------------------------------------------------------
    
    ### DateTime Tab2 ------------------------------------------------------
    
    #### Date window Tab2 ------------------------------------------------------
    
    ##### year ---------------------------------------------------------------#
    observeEvent(input$tab2_window_year_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 365*24*3600)
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 365*24*3600)
    })
    observeEvent(input$tab2_window_year_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 365*24*3600)
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 365*24*3600)
    })
    ##### month ---------------------------------------------------------------#
    observeEvent(input$tab2_window_month_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 30*24*3600)
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 30*24*3600)
    })
    observeEvent(input$tab2_window_month_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 30*24*3600)
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 30*24*3600)
    })
    ##### week ---------------------------------------------------------------#
    observeEvent(input$tab2_window_week_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 7*24*3600)
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 7*24*3600)
    })
    observeEvent(input$tab2_window_week_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 7*24*3600)
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 7*24*3600)
    })
    ##### day ---------------------------------------------------------------#
    observeEvent(input$tab2_window_day_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 24*3600)
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 24*3600)
    })
    observeEvent(input$tab2_window_day_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 24*3600)
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 24*3600)
    })
    
    #### datemin Tab2 ------------------------------------------------------
    
    ##### year ---------------------------------------------------------------#
    observeEvent(input$tab2_datemin_year_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 365*24*3600)
    })
    observeEvent(input$tab2_datemin_year_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 365*24*3600)
    })
    ##### month ---------------------------------------------------------------#
    observeEvent(input$tab2_datemin_month_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 30*24*3600)
    })
    observeEvent(input$tab2_datemin_month_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 30*24*3600)
    })
    ##### week ---------------------------------------------------------------#
    observeEvent(input$tab2_datemin_week_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 7*24*3600)
    })
    observeEvent(input$tab2_datemin_week_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 7*24*3600)
    })
    ##### day ---------------------------------------------------------------#
    observeEvent(input$tab2_datemin_day_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 24*3600)
    })
    observeEvent(input$tab2_datemin_day_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 24*3600)
    })
    ##### time ---------------------------------------------------------------#
    observeEvent(input$tab2_timemin_left, {
      updateSliderInput(session, "tab2_datemin", 
                        value = input$tab2_datemin - 1800)
    })
    observeEvent(input$tab2_timemin_right, {
      updateSliderInput(session, "tab2_datemin", 
                        value = input$tab2_datemin + 1800)
    })
    
    #### datemax Tab2 ------------------------------------------------------
    
    ##### year ---------------------------------------------------------------#
    observeEvent(input$tab2_datemax_year_left, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 365*24*3600)
    })
    observeEvent(input$tab2_datemax_year_right, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 365*24*3600)
    })    
    ##### month ---------------------------------------------------------------#
    observeEvent(input$tab2_datemax_month_left, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 30*24*3600)
    })
    observeEvent(input$tab2_datemax_month_right, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 30*24*3600)
    })
    ##### week ---------------------------------------------------------------#
    observeEvent(input$tab2_datemax_week_left, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 7*24*3600)
    })
    observeEvent(input$tab2_datemax_week_right, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 7*24*3600)
    })
    ##### day ---------------------------------------------------------------#
    observeEvent(input$tab2_datemax_day_left, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 24*3600)
    })
    observeEvent(input$tab2_datemax_day_right, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 24*3600)
    })
    ##### time ---------------------------------------------------------------#
    observeEvent(input$tab2_timemax_left, {
      updateSliderInput(session, "tab2_datemax", 
                        value = input$tab2_datemax - 1800)
    })
    observeEvent(input$tab2_timemax_right, {
      updateSliderInput(session, "tab2_datemax", 
                        value = input$tab2_datemax + 1800)
    })
    
    
    
    ### UI changes with tab2_type
    observeEvent({ 
      input$tab2_type
    }, {
      # Hide/Show UI for selected models and var
      if (input$tab2_type == "scatterplot_model") {
        reset("tab2_var2")
        hide("tab2_selected_output")
        hide("tab2_var2")
        shinyjs::show("tab2_selected_output2")
        shinyjs::show("tab2_scatterplot_points")
      } else if (input$tab2_type == "scatterplot_var") {
        hide("tab2_selected_output2")
        shinyjs::show("tab2_selected_output")
        shinyjs::show("tab2_var2")
        shinyjs::show("tab2_scatterplot_points")
        updateSelectInput(session, "tab2_var",
                          selected = input$tab2_var)
      } else {
        reset("tab2_var2")
        reset("tab2_scatterplot_points")
        hide("tab2_var2")
        hide("tab2_selected_output2")
        hide("tab2_scatterplot_points")
        shinyjs::show("tab2_selected_output")
      }
    })
    ### Selected Models -------------------------------------------------------
    
    observeEvent(input$tab1_selected_output, {
      updateSelectInput(session, "tab2_selected_output",
                        selected = input$tab1_selected_output)
    })
    
    observeEvent(input$tab2_selected_output, {
      updateSelectInput(session, "tab2_selected_output2.1",
                        selected = first(input$tab2_selected_output))
    })
    
    observeEvent(
      {
        input$tab2_selected_output2.1
      }, {
        avail.output <- names(x)
        avail.output <- avail.output[which(avail.output != input$tab2_selected_output2.1)]
        if (length(input$tab2_selected_output) > 1) {
          this.default <- input$tab2_selected_output[2]
        } else {
          this.default <- first(avail.output)
        }
        
        updateSelectInput(session, "tab2_selected_output2.2",
                          choices = avail.output, 
                          selected = this.default)
      })
    
    ### Selected Variables ----------------------------------------------------
    
    observeEvent(input$tab1_var1, {
      updateSelectInput(session, "tab2_var",
                        selected = input$tab1_var1)
    })
    
    observeEvent(
      {
        input$tab2_var
      }, {
        if (input$tab2_type %in% c("scatterplot_var")) {
          avail.var <- sort(var_with_same_dim(x, input$tab2_var))
          updateSelectizeInput(session, "tab2_var2",
                               choices = avail.var)
          if (!(input$tab2_var2 %in% avail.var)) {
            updateSelectizeInput(session, "tab2_var2",
                                 selected = first(avail.var))
          }
        }
      })
    
    observeEvent(
      {
        input$tab2_type
      }, {
        if (input$tab2_type %in% c("scatterplot_var")) {
          avail.var <- var_with_same_dim(x, input$tab2_var)
          updateSelectizeInput(session, "tab2_var2",
                               choices = avail.var,
                               selected = first(avail.var))
        }
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
    
    
    ### Subset dim input tab2 -------------------------------------------------
    
    
    observeEvent({
      input$tab2_subset
      input$tab2_var
    }, {
      toggle_subset_input(session, input, index.var = NULL, x,
                          tab = "tab2", hide = !input$tab2_subset)
    })
    
    ### available choices for tab2_type -------------------------------------------------
    observeEvent(
      priority = 1, 
      { 
        input$tab2_var 
      }, {
        this.var.dim <- get_variable(x[[1]],
                                     input$tab2_var,
                                     return.colnames = TRUE)
        current.selected <- input$tab2_type
        choices <- c("standard",
                     "daily_heatmap",
                     "boxplot",
                     "density",
                     "histogram",
                     "scatterplot_model",
                     "scatterplot_var")
        if (any(this.var.dim %in% c("nsoil","nveg","nair"))) {
          choices <- c("standard",
                       "heatmap",
                       "daily_heatmap",
                       "boxplot",
                       "density",
                       "histogram",
                       "scatterplot_model",
                       "scatterplot_var")
          
        }  else {
          if (current.selected == "heatmap") {
            current.selected <- "standard"
          }
        }
        updateSelectInput(session = session,
                          inputId = "tab2_type",
                          choices = choices,
                          selected = current.selected)
      })
    
    ### Toggle diffmodels -----------------------------------------
    observeEvent({
      input$tab2_selected_output
      input$tab2_type
    }, {
      if (length(input$tab2_selected_output) > 1 &
          !(input$tab2_type %in% c("scatterplot_var",
                                   "scatterplot_model") ))  {
        shinyjs::show("tab2_diffmodels")
      } else {
        reset("tab2_diffmodels")
        hide("tab2_diffmodels")
      }
    })
    
    
    ### x, y, colors, linetype, shape, fill, facet ------------------
    
    
    observeEvent({
      input$tab2_type
      input$tab2_var
      input$tab2_scatterplot_points
      tab2_testdf()
    }, {
      avail.dim <- attr(tab2_testdf(), "dimname")
      this.var <- attr(tab2_testdf(), "var")
      this.models <- attr(tab2_testdf(), "models")
      
      if (input$tab2_type %in% c("scatterplot_model")) {
        avail.dim <- str_subset(avail.dim, "models", negate = TRUE)
      }
      default.dim <- avail.dim
      discrete.dim <- str_subset(avail.dim,
                                 pattern = "nsoil|nair|nveg|models|nspecies|nleafage")
      
      #### x ------------------------------------------------------------------
      x.hide <- FALSE
      if (input$tab2_type %in% c("standard", "heatmap")) {
        x <- x.choices <- "time"
        avail.dim <- str_subset(avail.dim, "time", negate = TRUE)
        default.dim <- str_subset(default.dim, "time", negate = TRUE)
      } else if (input$tab2_type %in% c("scatterplot_var")) {
        x <- x.choices <- this.var[1]
      } else if (input$tab2_type %in% c("scatterplot_model")) {
        x <- x.choices <- this.models[1]
      } else if (input$tab2_type %in% c("boxplot")) {
        avail.dim <- str_subset(avail.dim, "time", negate = TRUE)
        x.choices <- c("", avail.dim)
        if (is.null(input$tab2_x) || !(input$tab2_x %in% avail.dim)) {
          x <- first(default.dim)
        } else {
          x <- input$tab2_x
        }
        default.dim <- str_subset(default.dim, x, negate = TRUE)
      } else if (input$tab2_type %in% c("density", "histogram")) {
        avail.dim <- str_subset(avail.dim, "time", negate = TRUE)
        x <- x.choices <- this.var
      } else if (input$tab2_type %in% c("daily_heatmap")) {
        avail.dim <- str_subset(avail.dim, "time", negate = TRUE)
        x <- x.choices <- "Time of day"
      }
      
      ### y --------------------------------------------------------------------
      
      y.hide <- FALSE
      if (input$tab2_type %in% c("standard", "boxplot")) {
        y <- y.choices <- this.var
      } else if (input$tab2_type %in% c("scatterplot_var")) {
        y <- y.choices <- this.var[2]
      } else if (input$tab2_type %in% c("scatterplot_model")) {
        y <- y.choices <- this.models[2]
      } else if (input$tab2_type %in% c("density", "histogram")) {
        y.hide <- TRUE
        y <- y.choices <- NULL
      } else if (input$tab2_type %in% c("heatmap")) {
        heatmap.dim <- str_subset(avail.dim,
                                  pattern = "nsoil|nair|nveg")
        if (length(heatmap.dim) > 0) {
          y.choices <- heatmap.dim
          if (is.null(input$tab2_y) || !(input$tab2_y %in% y.choices)) {
            y <- ifelse(is.null(heatmap.dim), NULL, first(heatmap.dim))
          } else {
            y <- input$tab2_y
          } 
          default.dim <- str_subset(default.dim, y, negate = TRUE)
        } else {
          y <- y.choices <- NULL
        }
        
      } else if (input$tab2_type %in% c("daily_heatmap")) {
        y <- y.choices <- "Julian day"
      }
      
      
      ### fill -----------------------------------------------------------------
      
      
      fill.hide <- FALSE
      if (input$tab2_type %in% c("standard", "density", 
                                 "scatterplot_var", "scatterplot_model")) {
        fill.hide <- TRUE
        fill <- fill.choices <- NULL
      } else if (input$tab2_type %in% c("boxplot", "histogram")) {
        if (length(discrete.dim) > 0) {
          fill.choices <- discrete.dim
          if (is.null(input$tab2_fill) || 
              !(input$tab2_fill %in% fill.choices)) {
            default.dim.fill <- str_subset(default.dim, paste0(fill.choices, collapse = "|"))
            fill <- ifelse(is.null(default.dim.fill), NULL, first(default.dim.fill))
          } else {
            fill <- input$tab2_fill
          }
          default.dim <- str_subset(default.dim, fill, negate = TRUE)
        } else {
          fill.hide <- TRUE
          fill <- fill.choices <- NULL
        }
        
        
      } else if (input$tab2_type %in% c("heatmap", "daily_heatmap")) {
        fill <- fill.choices <- this.var
      }    
      
      ### color ----------------------------------------------------------------
      
      color.hide <- FALSE
      if (input$tab2_type %in% c("standard",
                                 "density") |
          (input$tab2_type %in% c("scatterplot_var", 
                                  "scatterplot_model") &
           input$tab2_scatterplot_points)) {
        color.choices <- avail.dim
        if (is.null(input$tab2_color) ||
            !(input$tab2_color %in% color.choices)) {
          color <- first(default.dim)
        } else {
          color <- input$tab2_color
        }
        default.dim <- str_subset(default.dim, color, negate = TRUE)
      } else {
        color.hide <- TRUE
        color <- color.choices <- NULL
      }    
      
      ### linetype -------------------------------------------------------------
      
      linetype.hide <- FALSE
      if (input$tab2_type %in% c("standard",
                                 "density") &
          length(discrete.dim) > 0) {
        linetype.choices <- discrete.dim
        default.dim.linetype <- str_subset(default.dim, 
                                           paste0(linetype.choices, 
                                                  collapse = "|"))
        if (length(linetype.choices) > 0) {
          if (is.null(input$tab2_linetype) ||
              !(input$tab2_linetype %in% linetype.choices)) {
            linetype <- first(default.dim)
          } else {
            linetype <- input$tab2_linetype
          }
          default.dim <- str_subset(default.dim, linetype, negate = TRUE)
        } else {
          linetype <- NULL
        }
      } else {
        linetype.hide <- TRUE
        linetype <- linetype.choices <- NULL
      }    
      
      
      ### shape ----------------------------------------------------------------
      
      shape.hide <- FALSE
      if (input$tab2_type %in% c("scatterplot_var",
                                 "scatterplot_model") &
          input$tab2_scatterplot_points & 
          length(discrete.dim) > 0) {
        shape.choices <- discrete.dim
        default.dim.shape <- str_subset(default.dim,
                                        paste0(shape.choices, 
                                               collapse = "|"))
        if (length(default.dim.shape) > 0) {
          if (is.null(input$tab2_shape) || 
              !(input$tab2_shape %in% shape.choices)) {
            shape <- first(default.dim.shape)
          } else {
            shape <- input$tab2_shape
          }
          default.dim <- str_subset(default.dim, shape, negate = TRUE)
        } else {
          shape <- NULL
        }
      } else {
        shape.hide <- TRUE
        shape <- shape.choices <- NULL
      }
      
      ### updateSelectizeInput, hide/show  -------------------------------------
      
      for (this.param in c("x","y","color","shape","linetype","fill")) {
        this.param.choices <- paste0(this.param,".choices")
        this.param.hide <- paste0(this.param, ".hide")
        this.selectize <- paste0("tab2_",this.param)
        
        if (eval(sym(this.param.hide))) {
          reset(id = this.selectize)
          hide(id = this.selectize)
        } else {
          shinyjs::show(id = this.selectize)
          updateSelectizeInput(
            session = session,
            inputId = this.selectize,
            choices = eval(sym(this.param.choices)),
            selected = eval(sym(this.param)))
        }
        
      }
      
      ### tab2_facet  -------------------------------------
      
      if (length(discrete.dim) > 0) {
        if (!is.null(input$tab2_facet) &&
            all(input$tab2_facet %in% discrete.dim)) {
          facet <- input$tab2_facet
        } else if ("models" %in% discrete.dim &&
                   input$tab2_type %in% c("heatmap", 
                                          "daily_heatmap")) {
          facet <- "models" 
        } else {
          facet <- NULL
        }
        updateSelectInput(session = session,
                          "tab2_facet",
                          choices = discrete.dim,
                          selected = facet)
      } else {
        reset(id = "tab2_facet")
      }
    })
    
    
    
    ### tab2 range -----------------------------------------------------
    
    observeEvent({
      input$tab2_selected_output  
      input$tab2_type
    }, {
      
      #### xrange -------------------------------------
      if (is.null(input$tab2_type) ||
          !(input$tab2_type %in% c("scatterplot_model",
                                   "scatterplot_var",
                                   "density",
                                   "histogram"))) {
        reset("tab2_xmin")
        reset("tab2_xmax")
        hide("tab2_xrange")
      } else {
        shinyjs::show("tab2_xrange")
      }
      
      #### yrange -------------------------------------
      if (is.null(input$tab2_type) ||
          !(input$tab2_type %in% c("standard",
                                   "boxplot", 
                                   "heatmap",
                                   "scatterplot_model",
                                   "scatterplot_var",
                                   "density",
                                   "histogram"))) {
        reset("tab2_ymin")
        reset("tab2_ymax")
        hide("tab2_yrange")
      } else {
        shinyjs::show("tab2_yrange")
      }
      #### fillrange -------------------------------------
      if (is.null(input$tab2_type) ||
          !(input$tab2_type %in% c("heatmap",
                                   "daily_heatmap")))  {
        reset("tab2_fillmin")
        reset("tab2_fillmax")
        hide("tab2_fillrange") 
      } else {
        shinyjs::show("tab2_fillrange")
      }
    })
    
    
    #### Reset all ranges with change of var ----------------------------------------------
    
    observeEvent({
      input$tab2_var
    }, {
      reset(id = "tab2_fillmin", asis = FALSE)
      reset(id = "tab2_fillmax", asis = FALSE)
      reset(id = "tab2_ymin", asis = FALSE)
      reset(id = "tab2_ymax", asis = FALSE)
      reset(id = "tab2_xmin", asis = FALSE)
      reset(id = "tab2_xmax", asis = FALSE)
    })
    
    ### tab2 filename -----------------------------------------------------------
    
    observeEvent({
      input$tab2_var
      input$tab2_file_prefix
      input$tab2_file_suffix
      input$tab2_file_format
    }, {
      updateTextInput(session, "tab2_filename",
                      value = 
                        get_shiny_filename(
                          input$tab2_var,
                          input$tab2_file_prefix,
                          input$tab2_file_suffix,
                          input$tab2_file_format))
    })
    
    ## data and plot generation -----------------------------------------------
    
    
    ### tab2_df -----------------------------------------------------------
    tab2_df <- eventReactive({
      input$tab2_UpdateView
    }, {
      if (!(input$tab2_type %in% c("scatterplot_model"))) {
        this.selected <- input$tab2_selected_output
      } else {
        this.selected <- c(input$tab2_selected_output2.1,
                           input$tab2_selected_output2.2)
      }
      
      if (input$tab2_subset) {
        list.soil.level = input$tab2_nsoil
        list.air.level = input$tab2_nair
        list.species.level = input$tab2_nspecies
        list.veg.level = input$tab2_nveg
        list.leafage.level = input$tab2_nleafage
      } else {
        list.soil.level = NULL
        list.air.level = NULL
        list.species.level = NULL
        list.veg.level = NULL
        list.leafage.level = NULL
      }
      if (is.null(input$tab2_var2) || input$tab2_var2 == "") {
        df <- 
          get_variable_comparison(x[this.selected],
                                  varname = input$tab2_var,
                                  time_range = c(input$tab2_datemin, input$tab2_datemax),
                                  list.soil.level = list.soil.level,
                                  list.air.level = list.air.level,
                                  list.species.level = list.species.level,
                                  list.veg.level = list.veg.level,
                                  list.leafage.level = list.leafage.level,
                                  diffmodels = input$tab2_diffmodels)
      } else {
        df <- 
          get_two_variables(x[this.selected],
                            varnames = c(input$tab2_var, input$tab2_var2),
                            time_range = c(input$tab2_datemin, input$tab2_datemax),
                            list.soil.level = list.soil.level,
                            list.air.level = list.air.level,
                            list.species.level = list.species.level,
                            list.veg.level = list.veg.level,
                            list.leafage.level = list.leafage.level,
                            diffmodels = input$tab2_diffmodels)
        
      }
      df
    })
    
    
    ### tab2_testdf -----------------------------------------------------------
    tab2_testdf <- eventReactive(
      { 
        input$tab2_var
        input$tab2_var2
        input$tab2_diffmodels
        input$tab2_selected_output2.1
        input$tab2_selected_output2.2
      }, {
        if (!(input$tab2_type %in% c("scatterplot_model"))) {
          this.selected <- input$tab2_selected_output
        } else {
          this.selected <- c(input$tab2_selected_output2.1,
                             input$tab2_selected_output2.2)
        }
        
        if (is.null(input$tab2_var2) || input$tab2_var2 == "") {
          df <- 
            get_variable_comparison(x[this.selected],
                                    varname = input$tab2_var,
                                    time_range = c(input$tab2_datemin,
                                                   input$tab2_datemin + 48*3600),
                                    diffmodels = input$tab2_diffmodels)
        } else {
          df <- 
            get_two_variables(x[this.selected],
                              varnames = c(input$tab2_var, input$tab2_var2),
                              time_range = c(input$tab2_datemin,
                                             input$tab2_datemin + 48*3600),
                              diffmodels = input$tab2_diffmodels)
        }
        df
      })
    
    ### tab2_plot -----------------------------------------------------------
    
    tab2_plot <- eventReactive({
      input$tab2_UpdateView 
    }, {
      
      if (input$tab2_type %in% c("scatterplot_model",
                                 "scatterplot_var")) {
        this.type <- "scatterplot"
      } else {
        this.type <- input$tab2_type
      }
      ggplot_variable(tab2_df(),
                      out.type = this.type,
                      x = input$tab2_x,
                      y = input$tab2_y,
                      shape = input$tab2_shape,
                      fill = input$tab2_fill,
                      color = input$tab2_color,
                      linetype = input$tab2_linetype,
                      facet_formula = input$tab2_facet,
                      xrange = c(input$tab2_xmin, input$tab2_xmax),
                      yrange = c(input$tab2_ymin, input$tab2_ymax),
                      fillrange = c(input$tab2_fillmin, input$tab2_fillmax),
                      diffmodels = input$tab2_diffmodels,
                      bin2d = !input$tab2_scatterplot_points,
                      nrow.facet = input$tab2_nrow_facet)
    })
    output$tab2_plot <- 
      renderPlot(tab2_plot())
    
    ### File handling -----------------------------------------------------------
    
    tab2_filename <- eventReactive(input$tab2_filename, {
      input$tab2_filename
    })
    tab2_width <- eventReactive(input$tab2_width, {
      input$tab2_width
    })
    tab2_height <- eventReactive(input$tab2_height, {
      input$tab2_height
    })
    output$tab2_download <- downloadHandler(
      filename = function() { tab2_filename() },
      content = function(file) {
        save_plot(file, tab2_plot(), 
                  base_width = tab2_width()/cm(1), 
                  base_height = tab2_height()/cm(1))
      }
    )
  }) #end shinyserver
} # end