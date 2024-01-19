# musica_ui ---------------------------------------------------------
##' @name musica_ui
##' @author Remi Lemaire-Patin
##' 
##' @title Shiny UI for \code{\link{shinymusica}}
##' 
##' @description This function generates a shiny UI for \code{\link{shinymusica}}
##' 
##' @inheritParams shinymusica
##' 
##' @return
##' 
##' A \code{shiny ui} object
##' 
##' @family shiny
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' x.list <- list("model1" = x, "model2" = x)
##' x.ui <- musica_ui(x.list)
##' 
##' @importFrom shiny shinyUI fluidPage tabsetPanel tabPanel
##' sidebarLayout sidebarPanel fluidRow plotOutput checkboxInput
##' sliderInput selectInput uiOutput  actionButton mainPanel icon 
##' selectizeInput conditionalPanel downloadButton textInput textAreaInput
##' numericInput column
##' @importFrom shinyjs useShinyjs hidden
##' @importFrom shinyWidgets awesomeRadio materialSwitch
##' @importFrom dygraphs dygraphOutput
##' @importFrom htmltools HTML tags hr
##' @export
##' 
##' 

musica_ui <- function(x) {
  shinyUI( 
    fluidPage(
      useShinyjs(),
      tabsetPanel(
        
        # Tab 1 - Interactive time-series ---------------------------------------
        tabPanel("Interactive time-series", fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     ## Input ----------------------------------------------
                     ### time range ----------------------------------------------
                     HTML("<b>Time Window</b>"),
                     fluidRow(
                       column(6,
                              add_changedate_button(id = "tab1_window_year",
                                                    legend = "Year",
                                                    class = "myDateRow")),
                       column(6,
                              add_changedate_button(id = "tab1_window_month",
                                                    legend = "Month",
                                                    class = "myDateRow")),
                     ),
                     fluidRow(
                       column(6,
                              add_changedate_button(id = "tab1_window_week",
                                                    legend = "Week",
                                                    class = "myDateRow")),
                       column(6,
                              add_changedate_button(id = "tab1_window_day",
                                                    legend = "Day",
                                                    class = "myDateRow"))
                     ),
                     sliderInput("tab1_datemin", label = "Start Date",
                                 min = get_time_range(x)[1],
                                 max = get_time_range(x)[2],
                                 value = get_6month(x)[1],
                                 timeFormat = "%F",
                                 step = 24*3600),  
                     fluidRow(
                       column(6,
                              add_changedate_button(id = "tab1_datemin_year",
                                                    legend = "Year",
                                                    class = "myDateRow")),
                       column(6,
                              add_changedate_button(id = "tab1_datemin_month",
                                                    legend = "Month",
                                                    class = "myDateRow")),
                     ),
                     fluidRow(
                       column(6,
                              add_changedate_button(id = "tab1_datemin_week",
                                                    legend = "Week",
                                                    class = "myDateRow")),
                       column(6,
                              add_changedate_button(id = "tab1_datemin_day",
                                                    legend = "Day",
                                                    class = "myDateRow"))
                     ),
                     sliderInput("tab1_datemax", label = "End Date",
                                 min = get_time_range(x)[1],
                                 max = get_time_range(x)[2],
                                 value = get_6month(x)[2],
                                 timeFormat = "%F",
                                 step = 24*3600),  
                     fluidRow(
                       column(6,
                              add_changedate_button(id = "tab1_datemax_year",
                                                    legend = "Year",
                                                    class = "myDateRow")),
                       column(6,
                              add_changedate_button(id = "tab1_datemax_month",
                                                    legend = "Month",
                                                    class = "myDateRow")),
                     ),
                     fluidRow(
                       column(6,
                              add_changedate_button(id = "tab1_datemax_week",
                                                    legend = "Week",
                                                    class = "myDateRow")),
                       column(6,
                              add_changedate_button(id = "tab1_datemax_day",
                                                    legend = "Day",
                                                    class = "myDateRow"))
                     ),
                     ### Refresh ----------------------------------------
                     hr(), 
                     actionButton("tab1_UpdateView", icon("refresh")),
                     hr(),
                     ### selected models ----------------------------------------
                     selectInput("tab1_selected_output", label = "Outputs to plot",
                                 choices = names(x),
                                 multiple = TRUE,
                                 selected = first(names(x))),
                     ### variable1 ----------------------------------------------
                     fluidRow(
                       column(9,
                              selectInput("tab1_var1", label = "1st Variable",
                                          choices = sort(var_with_dim(x[[1]],"time")),
                                          selected = "Qle")),
                       column(3,
                              materialSwitch(
                                inputId = "tab1_plotvar1",
                                label = NULL, 
                                status = "primary",
                                value = TRUE
                              ),
                              hidden(
                                div(id = "tab1_diffmodels1",
                                    checkboxInput("tab1_diffmodels1", 
                                                  label = "Diff",
                                                  value = FALSE))))),
                     get_subset_input(x, "nsoil", "tab1_nsoil1"),
                     get_subset_input(x, "nair", "tab1_nair1"),
                     get_subset_input(x, "nspecies", "tab1_nspecies1"),
                     get_subset_input(x, "nveg", "tab1_nveg1"),
                     get_subset_input(x, "nleafage", "tab1_nleafage1"),
                     ### variable2 ----------------------------------------------
                     fluidRow(
                       column(9,
                              selectInput("tab1_var2", label = "2nd Variable",
                                          choices = sort(var_with_dim(x[[1]],"time")),
                                          selected = "Qh")),
                       column(3,
                              materialSwitch(
                                inputId = "tab1_plotvar2",
                                label = NULL, 
                                status = "primary",
                                value = FALSE
                              ),
                              hidden(
                                div(id = "tab1_diffmodels2",
                                    checkboxInput("tab1_diffmodels2", 
                                                  label = "Diff",
                                                  value = FALSE))))),
                     get_subset_input(x, "nsoil", "tab1_nsoil2"),
                     get_subset_input(x, "nair", "tab1_nair2"),
                     get_subset_input(x, "nspecies", "tab1_nspecies2"),
                     get_subset_input(x, "nveg", "tab1_nveg2"),
                     get_subset_input(x, "nleafage", "tab1_nleafage2"),
                     ### variable3 ----------------------------------------------
                     fluidRow(
                       column(9,
                              selectInput("tab1_var3", label = "3rd Variable",
                                          choices = sort(var_with_dim(x[[1]],"time")),
                                          selected = "Qg")),
                       column(3,
                              materialSwitch(
                                inputId = "tab1_plotvar3",
                                label = NULL, 
                                value = FALSE,
                                status = "primary"
                              ),
                              hidden(
                                div(id = "tab1_diffmodels3",
                                    checkboxInput("tab1_diffmodels3", 
                                                  label = "Diff",
                                                  value = FALSE))))),
                     get_subset_input(x, "nsoil", "tab1_nsoil3"),
                     get_subset_input(x, "nair", "tab1_nair3"),
                     get_subset_input(x, "nspecies", "tab1_nspecies3"),
                     get_subset_input(x, "nveg", "tab1_nveg3"),
                     get_subset_input(x, "nleafage", "tab1_nleafage3"),
                     # actionButton("tab1_StoreValue", icon("floppy-disk")),
                     width = 3
                   ),
                   ## Output ---------------------------------------------------
                   mainPanel(
                     dygraphOutput("tab1_dygraph1", height = "200px"),
                     dygraphOutput("tab1_dygraph2", height = "200px"),
                     dygraphOutput("tab1_dygraph3", height = "200px"),
                     width = 9
                   ),
                   position = "left"
                 )
        ), # end tabPanel1
        # Tab 2 - Static Graphics ------------------------------------
        tabPanel(
          "Static Graphics & Export", fluid = TRUE,
          sidebarLayout(
            sidebarPanel(
              ## Input --------------------------------------------------
              ## time range --------------------------------------------
              HTML("<b>Time Window</b>"),
              fluidRow(
                column(4,
                       add_changedate_button(id = "tab2_window_year",
                                             legend = "Year",
                                             class = "myDateRow")),
                column(4,
                       add_changedate_button(id = "tab2_window_month",
                                             legend = "Month",
                                             class = "myDateRow")),
                column(4)
              ),
              fluidRow(
                column(4,
                       add_changedate_button(id = "tab2_window_week",
                                             legend = "Week",
                                             class = "myDateRow")),
                column(4,
                       add_changedate_button(id = "tab2_window_day",
                                             legend = "Day",
                                             class = "myDateRow")),
                column(4)
              ),
              sliderInput("tab2_datemin", label = "Start Date",
                          min = get_time_range(x)[1],
                          max = get_time_range(x)[2],
                          value = get_6month(x)[1],
                          timeFormat = "%F %T",
                          step = 24*3600,
                          round = 4),
              fluidRow(
                column(4,
                       add_changedate_button(id = "tab2_datemin_year",
                                             legend = "Year",
                                             class = "myDateRow")),
                column(4,
                       add_changedate_button(id = "tab2_datemin_month",
                                             legend = "Month",
                                             class = "myDateRow")),
                column(4)
              ),
              fluidRow(
                column(4,
                       add_changedate_button(id = "tab2_datemin_week",
                                             legend = "Week",
                                             class = "myDateRow")),
                column(4,
                       add_changedate_button(id = "tab2_datemin_day",
                                             legend = "Day",
                                             class = "myDateRow")),
                column(4,
                       add_changedate_button(id = "tab2_timemin",
                                             legend = "Time step",
                                             class = "myDateRow"))
              ),
              sliderInput("tab2_datemax", label = "End Date",
                          min = get_time_range(x)[1],
                          max = get_time_range(x)[2],
                          value = get_6month(x)[2],
                          timeFormat = "%F %T",
                          step = 24*3600,
                          round = 4) ,
              fluidRow(
                column(4,
                       add_changedate_button(id = "tab2_datemax_year",
                                             legend = "Year",
                                             class = "myDateRow")),
                column(4,
                       add_changedate_button(id = "tab2_datemax_month",
                                             legend = "Month",
                                             class = "myDateRow")
                ),
                column(4)
              ),
              fluidRow(
                column(4,
                       add_changedate_button(id = "tab2_datemax_week",
                                             legend = "Week",
                                             class = "myDateRow")),
                column(4,
                       add_changedate_button(id = "tab2_datemax_day",
                                             legend = "Day",
                                             class = "myDateRow")),
                column(4,
                       add_changedate_button(id = "tab2_timemax",
                                             legend = "Time step",
                                             class = "myDateRow"))
              ),
              ## Update View & tab2_type -------------------------------------
              hr(), 
              fluidRow(
                column(6,
                       selectInput("tab2_type", label = "Plot type",
                                   choices = c("standard",
                                               "daily_heatmap",
                                               "heatmap",
                                               "boxplot",
                                               "density",
                                               "histogram",
                                               "scatterplot_model",
                                               "scatterplot_var"),
                                   selected = "standard")),
                column(3,
                       hidden(div(
                         id = "tab2_scatterplot_points",
                         checkboxInput("tab2_scatterplot_points", 
                                       label = "Points",
                                       value = FALSE)))
                ),
                column(3,
                       actionButton("tab2_UpdateView", icon("refresh"))
                )
              ),
              hr(), 
              ## models --------------------------------------------
              div(id = "tab2_selected_output", 
                  fluidRow(
                    column(8,
                           selectInput("tab2_selected_output", label = "Outputs to plot",
                                       choices = names(x),
                                       multiple = TRUE,
                                       selected = first(names(x)))),
                    column(4,
                           hidden(
                             div(id = "tab2_diffmodels",
                                 checkboxInput("tab2_diffmodels", 
                                               label = "Difference",
                                               value = FALSE)))
                    ))),
              ## models 2 --------------------------------------------
              hidden(
                div(id = "tab2_selected_output2",
                    fluidRow(
                      column(6, 
                             selectInput("tab2_selected_output2.1", label = "First model",
                                         choices = names(x),
                                         multiple = FALSE,
                                         selected = first(names(x)))),
                      column(6,
                             selectInput("tab2_selected_output2.2", label = "Second model",
                                         choices = names(x),
                                         multiple = FALSE,
                                         selected = names(x)[2])),
                    ))),
              ## variable --------------------------------------------
              fluidRow(
                column(6, 
                       selectInput("tab2_var", label = "Variable",
                                   choices = sort(var_with_dim(x[[1]],"time")),
                                   selected = "Qg")),
                column(6,
                       hidden(
                         div(id = "tab2_var2",
                             selectizeInput(
                               "tab2_var2", 
                               label = "Variable 2",
                               choices = NULL,
                               options = list(
                                 placeholder = 'Empty',
                                 onInitialize =
                                   I('function() { this.setValue(""); }')))
                         ))
                )),
              ## subset options --------------------------------------------
              checkboxInput("tab2_subset", "subset data", value = FALSE),
              # subset options
              get_subset_input(x, "nsoil", "tab2_nsoil", multiple = TRUE),
              get_subset_input(x, "nair", "tab2_nair", multiple = TRUE),
              get_subset_input(x, "nspecies", "tab2_nspecies", multiple = TRUE),
              get_subset_input(x, "nveg", "tab2_nveg", multiple = TRUE),
              get_subset_input(x, "nleafage", "tab2_nleafage", multiple = TRUE),
              get_range_input("tab2","x"),
              get_range_input("tab2","y"),
              get_range_input("tab2","fill"),
              # end subset options
              div(id = "tab2_facet", 
                  fluidRow(
                    column(8,
                           selectInput("tab2_facet", label = "facet",
                                       choices = NULL,
                                       multiple = TRUE,
                                       selected = NULL)),
                    column(4,
                           numericInput("tab2_nrow_facet", 
                                        label = "rows",
                                        value = 1)
                    ))),
              hidden(div(
                id = "tab2_x",
                selectizeInput("tab2_x", label = "x",
                               choices = NULL,
                               options = list(
                                 placeholder = 'Empty',
                                 onInitialize =
                                   I('function() { this.setValue(""); }')))
              )),
              hidden(div(
                id = "tab2_y",
                selectizeInput("tab2_y", label = "y",
                               choices = NULL,
                               options = list(
                                 placeholder = 'Empty',
                                 onInitialize =
                                   I('function() { this.setValue(""); }')))
              )),
              hidden(div(
                id = "tab2_color",
                selectizeInput("tab2_color", label = "color",
                               choices = NULL,
                               options = list(
                                 placeholder = 'Empty',
                                 onInitialize =
                                   I('function() { this.setValue(""); }')))
              )),
              hidden(div(
                id = "tab2_linetype",
                selectizeInput("tab2_linetype", label = "linetype",
                               choices = NULL,
                               options = list(
                                 placeholder = 'Empty',
                                 onInitialize = 
                                   I('function() { this.setValue(""); }')))
              )),
              hidden(div(
                id = "tab2_fill",
                selectizeInput("tab2_fill", label = "fill",
                               choices = NULL,
                               options = list(
                                 placeholder = 'Empty',
                                 onInitialize = 
                                   I('function() { this.setValue(""); }')))
              )),
              hidden(div(
                id = "tab2_shape",
                selectizeInput("tab2_shape", label = "shape",
                               choices = NULL,
                               options = list(
                                 placeholder = 'Empty',
                                 onInitialize =
                                   I('function() { this.setValue(""); }')))
              )),
              hr(), 
              downloadButton('tab2_download', "Download Plot"),
              selectInput("tab2_file_format", "File Format",
                          choices = c("png","pdf","svg","jpeg"),
                          selected = "png"),
              textInput("tab2_file_prefix", "Filename prefix",
                        value = ""),
              textInput("tab2_file_suffix", "Filename suffix",
                        value = ""),
              textAreaInput("tab2_filename", "Full Filename",
                            value = "", height = "25px"),
              fluidRow(
                column(6,
                       numericInput("tab2_width", "Fig. width (cm)",
                                    min = 0, 
                                    value = 16,
                                    width = "100%")),
                column(6,
                       numericInput("tab2_height", "Fig. height (cm)",
                                    min = 0, 
                                    value = 12,
                                    width = "100%"))),
              width = 3
            ),
            ## Output ---------------------------------------------------
            mainPanel(
              plotOutput("tab2_plot", height = "650px"),
              width = 9
            ),
            position = "left"
          )
        ) # end tabPanel 2
      ), # end tabsetPanel
      
      # HTML Tags ---------------------------------------------------------------
      
      tags$head(tags$style(HTML(".shiny-input-container > label {
                            margin-bottom: -15px
                            margin-top: -25px
                          }")),
                tags$style(HTML(".dygraph-label {
                            font-size: 14px;
                          }")),
                tags$style(HTML(".selectize-input {
                            height = 40px;
                            font-size: 12px;
                            line-height: 12px;
                            min-height: 10px;
                            padding-top: 0px;
                            margin: -10px
                          }")),
                tags$style(HTML(".myDateRow{
                                font-size: 8px;
                                line-height: 10px;
                                min-height: 10px;}")),
                tags$style(HTML("hr {border-top: 1px solid #000000;}")))
    )
  )
}



