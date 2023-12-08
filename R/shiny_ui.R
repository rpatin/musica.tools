# musica_ui ---------------------------------------------------------
##' @name musica_ui
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
##' @importFrom shiny shinyUI fluidPage tabsetPanel tabPanel sidebarLayout sidebarPanel
##' sliderInput selectInput uiOutput  actionButton mainPanel icon
##' @importFrom dygraphs dygraphOutput
##' @importFrom htmltools HTML tags
##' @export
##' 
##' 

musica_ui <- function(x) {
  shinyUI( 
    fluidPage(
      tabsetPanel(
        tabPanel("Interactive time-series", fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("tab1_time_range", label = "Time Range",
                                 min = get_time_range(x)[1],
                                 max = get_time_range(x)[2],
                                 value = get_6month(x),
                                 timeFormat = "%F",
                                 step = 24*3600),  
                     selectInput("tab1_selected_output", label = "Outputs to plot",
                                 choices = names(x),
                                 multiple = TRUE,
                                 selected = first(names(x))),
                     selectInput("tab1_var1", label = "1st Variable",
                                 choices = var_with_dim(x[[1]],"time"),
                                 selected = "Qle"),
                     uiOutput(outputId = 'tab1_dynamic_nsoil1'),
                     uiOutput(outputId = 'tab1_dynamic_nair1'),
                     uiOutput(outputId = 'tab1_dynamic_nspecies1'),
                     uiOutput(outputId = 'tab1_dynamic_nveg1'),
                     uiOutput(outputId = 'tab1_dynamic_nleafage1'),
                     selectInput("tab1_var2", label = "2nd Variable",
                                 choices = var_with_dim(x[[1]],"time"),
                                 selected = "Qh"),
                     uiOutput(outputId = 'tab1_dynamic_nsoil2'),
                     uiOutput(outputId = 'tab1_dynamic_nair2'),
                     uiOutput(outputId = 'tab1_dynamic_nspecies2'),
                     uiOutput(outputId = 'tab1_dynamic_nveg2'),
                     uiOutput(outputId = 'tab1_dynamic_nleafage2'),
                     selectInput("tab1_var3", label = "3rd Variable",
                                 choices = var_with_dim(x[[1]],"time"),
                                 selected = "Qg"),
                     uiOutput(outputId = 'tab1_dynamic_nsoil3'),
                     uiOutput(outputId = 'tab1_dynamic_nair3'),
                     uiOutput(outputId = 'tab1_dynamic_nspecies3'),
                     uiOutput(outputId = 'tab1_dynamic_nveg3'),
                     uiOutput(outputId = 'tab1_dynamic_nleafage3'),
                     actionButton("tab1_UpdateView", icon("refresh")),
                     width = 2
                   ),
                   mainPanel(
                     dygraphOutput("tab1_dygraph1", height = "200px"),
                     dygraphOutput("tab1_dygraph2", height = "200px"),
                     dygraphOutput("tab1_dygraph3", height = "200px"),
                     width = 10
                   ),
                   position = "left"
                 )
        ), # end tabPanel1
        tabPanel("Model to model comparison", fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("tab2_time_range", label = "Time Range",
                                 min = get_time_range(x)[1],
                                 max = get_time_range(x)[2],
                                 value = get_6month(x),
                                 timeFormat = "%F",
                                 step = 24*3600),  
                     width = 2
                   ),
                   mainPanel(
                     width = 10
                   ),
                   position = "left"
                 )
        ) # end tabPanel 2
      ), # end tabsetPanel
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
                          }")))
    )
  )
}



