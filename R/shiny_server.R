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
##' @importFrom shinyjs reset
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
    
    #### datemin Tab1 ------------------------------------------------------------
    
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
    observeEvent(input$tab1_datemin, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab1_datemin)
    })
    
    
    #### datemax Tab1 ------------------------------------------------------------
    
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
    observeEvent(input$tab1_datemax, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab1_datemax)
    })
    ### soil Tab1 ------------------------------------------------------------
    observeEvent(input$tab1_datemax_day_left, {
      updateSliderInput(session, "tab1_datemax", 
                        value = input$tab1_datemax - 24*3600,
                        timeFormat = "%F")
    })
    
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
                              time_range = c(input$tab1_datemin, input$tab1_datemax),
                              list.soil.level = input$tab1_nsoil1,
                              list.air.level = input$tab1_nair1,
                              list.species.level = input$tab1_nspecies1,
                              list.veg.level = input$tab1_nveg1,
                              list.leafage.level = input$tab1_nleafage1)
    })
    tab1_df2 <- eventReactive(input$tab1_UpdateView, {
      get_variable_comparison(x[input$tab1_selected_output],input$tab1_var2,
                              time_range = c(input$tab1_datemin, input$tab1_datemax),
                              list.soil.level = input$tab1_nsoil2,
                              list.air.level = input$tab1_nair2,
                              list.species.level = input$tab1_nspecies2,
                              list.veg.level = input$tab1_nveg2,
                              list.leafage.level = input$tab1_nleafage2)
    })
    tab1_df3 <- eventReactive(input$tab1_UpdateView, {
      get_variable_comparison(x[input$tab1_selected_output],input$tab1_var3,
                              time_range = c(input$tab1_datemin, input$tab1_datemax),
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
    
    #### datemin Tab2 ------------------------------------------------------
    
    observeEvent(input$tab2_datemin_month_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 30*24*3600)
    })
    observeEvent(input$tab2_datemin_month_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 30*24*3600)
    })
    observeEvent(input$tab2_datemin_week_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 7*24*3600)
    })
    observeEvent(input$tab2_datemin_week_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 7*24*3600)
    })
    observeEvent(input$tab2_datemin_day_left, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin - 24*3600)
    })
    observeEvent(input$tab2_datemin_day_right, {
      updateSliderInput(session, "tab2_datemin",
                        value = input$tab2_datemin + 24*3600)
    })
    
    observeEvent(input$tab2_timemin_left, {
      updateSliderInput(session, "tab2_datemin", 
                        value = input$tab2_datemin - 1800)
    })
    observeEvent(input$tab2_timemin_right, {
      updateSliderInput(session, "tab2_datemin", 
                        value = input$tab2_datemin + 1800)
    })
    
    #### datemax Tab2 ------------------------------------------------------
    
    observeEvent(input$tab2_datemax_month_left, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 30*24*3600)
    })
    observeEvent(input$tab2_datemax_month_right, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 30*24*3600)
    })
    observeEvent(input$tab2_datemax_week_left, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 7*24*3600)
    })
    observeEvent(input$tab2_datemax_week_right, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 7*24*3600)
    })
    observeEvent(input$tab2_datemax_day_left, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax - 24*3600)
    })
    observeEvent(input$tab2_datemax_day_right, {
      updateSliderInput(session, "tab2_datemax",
                        value = input$tab2_datemax + 24*3600)
    })
    
    observeEvent(input$tab2_timemax_left, {
      updateSliderInput(session, "tab2_datemax", 
                        value = input$tab2_datemax - 1800)
    })
    observeEvent(input$tab2_timemax_right, {
      updateSliderInput(session, "tab2_datemax", 
                        value = input$tab2_datemax + 1800)
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
    
    
    output$tab2_dynamic_datemin <-
      renderUI({
        eventReactive(input$tab1_datemin, {
          sliderInput("tab2_datemin", label = "Start Date",
                      min = get_time_range(x)[1],
                      max = get_time_range(x)[2],
                      value = input$tab1_datemin,
                      timeFormat = "%F %T",
                      step = 24*3600,
                      round = 4) 
        })()})
    
    output$tab2_dynamic_datemax <-
      renderUI({
        eventReactive(input$tab1_datemax, {
          sliderInput("tab2_datemax", label = "End Date",
                      min = get_time_range(x)[1],
                      max = get_time_range(x)[2],
                      value = input$tab1_datemax,
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
    ### tab2_dynamic_type -------------------------------------------------
    
    output$tab2_dynamic_type <-
      renderUI(
        reactive({
          function(this.input, this.selected.models){ 
            this.var.dim <- get_variable(musica.list[[1]],
                                         this.input,
                                         return.colnames = TRUE)
            this.choices <- c("standard",
                              "daily_heatmap")
            default.selected <- "standard"
            if (any(this.var.dim %in% c("nsoil","nveg","nair"))) {
              default.selected <- "heatmap"
              this.choices <- c(this.choices,
                                "heatmap")
              
            } 
            if (length(this.selected.models) > 1) {
              this.choices <- c(this.choices,
                                "boxplot",
                                "scatterplot")
            }
            selectInput("tab2_type", label = "Plot type",
                        choices = this.choices,
                        selected = default.selected)
          }
        })()(input$tab2_var, input$tab2_selected_output))
    
    ### tab2_dynamic_facet -------------------------------------------------
    
    output$tab2_dynamic_facet <-
      renderUI(
        reactive({
          function(this.var, this.type, this.selected.models){ 
            this.var.dim <- get_variable(musica.list[[this.selected.models[1]]],
                                         this.var,
                                         return.colnames = TRUE)
            default.selected = ""
            if (length(this.selected.models) > 1) {
              this.var.dim <- c("models", this.var.dim)
              if (this.type %in% c("heatmap", "daily_heatmap")) {
                default.selected = "models"
              }
            }
            selectInput("tab2_facet", label = "facet",
                        choices = this.var.dim,
                        multiple = TRUE,
                        selected = default.selected)
          }
        })()(input$tab2_var, input$tab2_type, input$tab2_selected_output))
    
    observeEvent({
      input$tab2_type
      input$tab2_selected_output
    }, {
      if (!is.null(input$tab2_type) &&
          input$tab2_type == "standard" && 
          length(input$tab2_selected_output) > 1)  {
        updateSelectInput(session = session,
                          "tab2_color",
                          selected = "models")
      }
    })
    
    ### tab2 xrange -----------------------------------------------------
    
    observeEvent({
      input$tab2_selected_output  
      input$tab2_type
    }, {
      if (!(!is.null(input$tab2_type)) &&
          input$tab2_type == "scatterplot" &&
          length(input$tab2_selected_output) > 1)  {
        reset(id = "tab2_xmin", asis = FALSE)
        reset(id = "tab2_xmax", asis = FALSE)
      }
    })
    
    ### tab2 yrange -----------------------------------------------------
    
    observeEvent({
      input$tab2_selected_output  
      input$tab2_type
    }, {
      if (!(!is.null(input$tab2_type) &&
            input$tab2_type %in% c("standard",
                                   "boxplot", 
                                   "heatmap",
                                   "scatterplot")))  {
        reset(id = "tab2_ymin", asis = FALSE)
        reset(id = "tab2_ymax", asis = FALSE)
      }
    })
    
    ### tab2 fillrange -----------------------------------------------------
    
    observeEvent({
      input$tab2_selected_output  
      input$tab2_type
    }, {
      if (!(!is.null(input$tab2_type) &&
            input$tab2_type %in% c("heatmap",
                                   "daily_heatmap")))  {
        reset(id = "tab2_fillmin", asis = FALSE)
        reset(id = "tab2_fillmax", asis = FALSE)
      }
    })
    
    
    #### Reset with change of var ----------------------------------------------
    
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
    tab2_df <- eventReactive(input$tab2_UpdateView, {
      if (input$tab2_subset) {
        df <- 
          get_variable_comparison(x[input$tab2_selected_output],
                                  this_var = input$tab2_var,
                                  time_range = c(input$tab2_datemin, input$tab2_datemax),
                                  list.soil.level = input$tab2_nsoil,
                                  list.air.level = input$tab2_nair,
                                  list.species.level = input$tab2_nspecies,
                                  list.veg.level = input$tab2_nveg,
                                  list.leafage.level = input$tab2_nleafage)
      } else {
        df <- 
          get_variable_comparison(x[input$tab2_selected_output],
                                  this_var = input$tab2_var,
                                  time_range = c(input$tab2_datemin, input$tab2_datemax),
                                  diffmodels = input$tab2_diffmodels)
      }
      df
    })
    
    tab2_plot <- eventReactive(input$tab2_UpdateView, {
      ggplot_variable(tab2_df(),
                      out.type = input$tab2_type,
                      color = input$tab2_color,
                      linetype = input$tab2_linetype,
                      facet_formula = input$tab2_facet,
                      xrange = c(input$tab2_xmin, input$tab2_xmax),
                      yrange = c(input$tab2_ymin, input$tab2_ymax),
                      fillrange = c(input$tab2_fillmin, input$tab2_fillmax),
                      diffmodels = input$tab2_diffmodels)
    })
    output$tab2_plot <- 
      renderPlot(tab2_plot())
    
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