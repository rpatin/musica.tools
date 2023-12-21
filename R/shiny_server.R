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
##' @importFrom shinyjs reset show hide toggle
##' @importFrom dygraphs renderDygraph
##' @importFrom htmltools div
##' @importFrom lubridate `year<-` year `day<-` day `month<-` month
##' @importFrom cowplot save_plot
##' @importFrom stringr str_subset
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
    
    
    tab1_df1 <- eventReactive(input$tab1_UpdateView, {
      get_variable_comparison(x[input$tab1_selected_output],input$tab1_var1,
                              time_range = c(input$tab1_datemin, input$tab1_datemax),
                              list.soil.level = input$tab1_nsoil1,
                              list.air.level = input$tab1_nair1,
                              list.species.level = input$tab1_nspecies1,
                              list.veg.level = input$tab1_nveg1,
                              list.leafage.level = input$tab1_nleafage1,
                              diffmodels = input$tab1_diffmodels1)
    })
    tab1_df2 <- eventReactive(input$tab1_UpdateView, {
      get_variable_comparison(x[input$tab1_selected_output],input$tab1_var2,
                              time_range = c(input$tab1_datemin, input$tab1_datemax),
                              list.soil.level = input$tab1_nsoil2,
                              list.air.level = input$tab1_nair2,
                              list.species.level = input$tab1_nspecies2,
                              list.veg.level = input$tab1_nveg2,
                              list.leafage.level = input$tab1_nleafage2,
                              diffmodels = input$tab1_diffmodels2)
    })
    tab1_df3 <- eventReactive(input$tab1_UpdateView, {
      get_variable_comparison(x[input$tab1_selected_output],input$tab1_var3,
                              time_range = c(input$tab1_datemin, input$tab1_datemax),
                              list.soil.level = input$tab1_nsoil3,
                              list.air.level = input$tab1_nair3,
                              list.species.level = input$tab1_nspecies3,
                              list.veg.level = input$tab1_nveg3,
                              list.leafage.level = input$tab1_nleafage3,
                              diffmodels = input$tab1_diffmodels3)
    })
    
    
    ## output Tab1 -------------------------------------------------------
    
    
    output$tab1_dygraph1 <- renderDygraph({
      dygraph_comparison(tab1_df1(),
                         main.title = tab1_main.title1(),
                         pixwidth = 600, 
                         pixheight = 40,
                         diffmodels = input$tab1_diffmodels1)
    })
    output$tab1_dygraph2 <- renderDygraph({
      dygraph_comparison(tab1_df2(),
                         main.title = tab1_main.title2(),
                         pixwidth = 600, 
                         pixheight = 40,
                         diffmodels = input$tab1_diffmodels2)
    })
    output$tab1_dygraph3 <- renderDygraph({
      dygraph_comparison(tab1_df3(),
                         main.title = tab1_main.title3(),
                         pixwidth = 600, 
                         pixheight = 40,
                         diffmodels = input$tab1_diffmodels3)
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
    
    ### Toggle subset ~ tab2_type ---------------------------------------------
    observeEvent(input$tab2_type, {
      if (!is.null(input$tab2_type) &&
          input$tab2_type == "heatmap") {
        updateCheckboxInput(session, "tab2_subset",
                            value = FALSE)
      }
    })
    ### Subset dim input tab2 -------------------------------------------------
    
    
    observeEvent({
      input$tab2_subset
      input$tab2_var
      input$tab2_subset
    }, {
      toggle_subset_input(session, input, index.var = NULL, x,
                          tab = "tab2", hide = !input$tab2_subset)
    })
    
    ### tab2_type -------------------------------------------------
    observeEvent({ 
      input$tab2_var 
      input$tab2_selected_output
    }, {
      this.var.dim <- get_variable(x[[1]],
                                   input$tab2_var,
                                   return.colnames = TRUE)
      this.choices <- c("standard",
                        "daily_heatmap")
      default.selected <- "standard"
      if (any(this.var.dim %in% c("nsoil","nveg","nair"))) {
        default.selected <- "heatmap"
        this.choices <- c(this.choices,
                          "heatmap")
        
      } 
      if (length(input$tab2_selected_output) > 1) {
        this.choices <- c(this.choices,
                          "distribution",
                          "scatterplot")
      }
      updateSelectInput(session = session,
                        inputId = "tab2_type",
                        choices = this.choices,
                        selected = default.selected)
    })
    
    #### tab2 type options -----------------------------------------------------
    observeEvent({ 
      input$tab2_type
    }, {
      if (!is.null(input$tab2_type) &&
          input$tab2_type == "scatterplot") {
        shinyjs::show("tab2_scatterplot_points")
      } else {
        # reset("tab2_scatterplot_points")
        hide("tab2_scatterplot_points")
      }
      
      if (!is.null(input$tab2_type) &&
          input$tab2_type == "distribution") {
        shinyjs::show("tab2_distribution_option")
      } else {
        # reset("tab2_distribution_option")
        hide("tab2_distribution_option")
      }
    })
    
    ### tab2_facet -------------------------------------------------
    
    
    observeEvent({
      input$tab2_var
      input$tab2_type
      input$tab2_selected_output
      input$tab2_diffmodels
    }, {
      this.var.dim <- get_variable(musica.list[[input$tab2_selected_output[1]]],
                                   input$tab2_var,
                                   return.colnames = TRUE)
      default.selected = ""
      if (length(input$tab2_selected_output) > 1) {
        this.var.dim <- c("models", this.var.dim)
        if (!is.null(input$tab2_type) &&
            input$tab2_type %in% c("heatmap", "daily_heatmap") &
            !input$tab2_diffmodels) {
          default.selected = "models"
        }
      }
      updateSelectInput(session = session,
                        "tab2_facet",
                        choices = this.var.dim,
                        selected = default.selected)
    })
    
    
    ### Toggle diffmodels -----------------------------------------
    observeEvent({
      input$tab2_selected_output
    }, {
      if (length(input$tab2_selected_output) > 1)  {
        shinyjs::show("tab2_diffmodels")
      } else {
        reset("tab2_diffmodels")
        hide("tab2_diffmodels")
      }
    })
    
    
    ### colors and linetype -----------------------------------------------------
    
    
    observeEvent({
      input$tab2_var
      input$tab2_type
      input$tab2_diffmodels
      input$tab2_distribution_option
      input$tab2_selected_output
    }, {
      this.var.dim <- get_variable(musica.list[[input$tab2_selected_output[1]]],
                                   input$tab2_var,
                                   return.colnames = TRUE)
      this.var.dim <- str_subset(this.var.dim, "time", negate = TRUE)
      this.selected <- first(this.var.dim)
      if (length(input$tab2_selected_output) > 1 &&
          !input$tab2_diffmodels) {
        this.var.dim <- c("models", this.var.dim)
        this.selected <- "models"
      }

      if (!is.null(input$tab2_type) &&
          (input$tab2_type == "standard" |
           (input$tab2_type == "distribution" &&
            input$tab2_distribution_option == c("Density"))
          ) &&
          length(this.var.dim) >= 1) {
        shinyjs::show("tab2_color")
        shinyjs::show("tab2_linetype")
        updateSelectizeInput(session = session,
                          "tab2_color",
                          choices = this.var.dim,
                          selected = this.selected)
        updateSelectizeInput(session = session,
                          "tab2_linetype",
                          choices = this.var.dim)
      } else {
        reset("tab2_color")
        reset("tab2_linetype")
        hide("tab2_color")
        hide("tab2_linetype")
      }
      
      if (!is.null(input$tab2_type) &&
          (input$tab2_type == "distribution" &&
           input$tab2_distribution_option %in% c("Boxplot",
                                                 "Histogram") &&
           !input$tab2_diffmodels)  &&
          length(this.var.dim) >= 1
      ) {
        shinyjs::show("tab2_fill")
        updateSelectizeInput(session = session,
                          "tab2_fill",
                          choices = this.var.dim,
                          selected = this.selected)
      } else {
        reset("tab2_fill")
        hide("tab2_fill")
      }
    })
    
    
    ### tab2 range -----------------------------------------------------
    
    observeEvent({
      input$tab2_selected_output  
      input$tab2_type
    }, {
      
      #### xrange -------------------------------------
      if (is.null(input$tab2_type) ||
          input$tab2_type != "scatterplot") {
        reset("tab2_xmin")
        reset("tab2_xmax")
        hide("tab2_xrange")
      } else {
        shinyjs::show("tab2_xrange")
      }
      
      #### yrange -------------------------------------
      if (is.null(input$tab2_type) ||
          !(input$tab2_type %in% c("standard",
                                   "distribution", 
                                   "heatmap",
                                   "scatterplot"))) {
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
                                  list.leafage.level = input$tab2_nleafage,
                                  diffmodels = input$diffmodels)
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