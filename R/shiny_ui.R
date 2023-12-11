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
##' @importFrom shiny shinyUI fluidPage tabsetPanel tabPanel
##' sidebarLayout sidebarPanel fluidRow plotOutput checkboxInput
##' sliderInput selectInput uiOutput  actionButton mainPanel icon 
##' selectizeInput conditionalPanel downloadButton
##' @importFrom dygraphs dygraphOutput
##' @importFrom htmltools HTML tags
##' @export
##' 
##' 

musica_ui <- function(x) {
  shinyUI( 
    fluidPage(
      tabsetPanel(
        
        # Tab 1 - Interactive time-series ---------------------------------------
        tabPanel("Interactive time-series", fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     ## Input ----------------------------------------------
                     ### time range ----------------------------------------------
                     sliderInput("tab1_time_range", label = "Time Range",
                                 min = get_time_range(x)[1],
                                 max = get_time_range(x)[2],
                                 value = get_6month(x),
                                 timeFormat = "%F",
                                 step = 24*3600),  
                     fluidRow(class = "myDateRow",
                              actionButton("tab1_datemin_left",
                                           icon("minus"),
                                           style='padding:4px; font-size:70%'),
                              "Start Date",
                              actionButton("tab1_datemin_right",
                                           icon("plus"),
                                           style='padding:4px; font-size:70%')),
                     fluidRow(class = "myDateRow",
                              actionButton("tab1_datemax_left",
                                           icon("minus"),
                                           style='padding:4px; font-size:70%'),
                              "End Date",
                              actionButton("tab1_datemax_right",
                                           icon("plus"),
                                           style='padding:4px; font-size:70%')),
                     ### selected models ----------------------------------------
                     selectInput("tab1_selected_output", label = "Outputs to plot",
                                 choices = names(x),
                                 multiple = TRUE,
                                 selected = first(names(x))),
                     ### variable1 ----------------------------------------------
                     selectInput("tab1_var1", label = "1st Variable",
                                 choices = var_with_dim(x[[1]],"time"),
                                 selected = "Qle"),
                     uiOutput(outputId = 'tab1_dynamic_nsoil1'),
                     uiOutput(outputId = 'tab1_dynamic_nair1'),
                     uiOutput(outputId = 'tab1_dynamic_nspecies1'),
                     uiOutput(outputId = 'tab1_dynamic_nveg1'),
                     uiOutput(outputId = 'tab1_dynamic_nleafage1'),
                     ### variable2 ----------------------------------------------
                     selectInput("tab1_var2", label = "2nd Variable",
                                 choices = var_with_dim(x[[1]],"time"),
                                 selected = "Qh"),
                     uiOutput(outputId = 'tab1_dynamic_nsoil2'),
                     uiOutput(outputId = 'tab1_dynamic_nair2'),
                     uiOutput(outputId = 'tab1_dynamic_nspecies2'),
                     uiOutput(outputId = 'tab1_dynamic_nveg2'),
                     uiOutput(outputId = 'tab1_dynamic_nleafage2'),
                     ### variable3 ----------------------------------------------
                     selectInput("tab1_var3", label = "3rd Variable",
                                 choices = var_with_dim(x[[1]],"time"),
                                 selected = "Qg"),
                     uiOutput(outputId = 'tab1_dynamic_nsoil3'),
                     uiOutput(outputId = 'tab1_dynamic_nair3'),
                     uiOutput(outputId = 'tab1_dynamic_nspecies3'),
                     uiOutput(outputId = 'tab1_dynamic_nveg3'),
                     uiOutput(outputId = 'tab1_dynamic_nleafage3'),
                     actionButton("tab1_UpdateView", icon("refresh")),
                     # actionButton("tab1_StoreValue", icon("floppy-disk")),
                     width = 2
                   ),
                   ## Output ---------------------------------------------------
                   mainPanel(
                     dygraphOutput("tab1_dygraph1", height = "200px"),
                     dygraphOutput("tab1_dygraph2", height = "200px"),
                     dygraphOutput("tab1_dygraph3", height = "200px"),
                     width = 10
                   ),
                   position = "left"
                 )
        ), # end tabPanel1
        # Tab 2 - Model to model comparison ------------------------------------
        tabPanel(
          "Model to model comparison", fluid = TRUE,
          sidebarLayout(
            sidebarPanel(
              ## Input --------------------------------------------------
              ## time range --------------------------------------------
              uiOutput(outputId = 'tab2_dynamic_date'),
              fluidRow(class = "myDateRow",
                       actionButton("tab2_date_left",
                                    icon("minus"),
                                    style='padding:4px; font-size:70%'),
                       "1 week",
                       actionButton("tab2_date_right",
                                    icon("plus"),
                                    style='padding:4px; font-size:70%')),
              fluidRow(class = "myDateRow",
                       actionButton("tab2_datemin_left",
                                     icon("minus"),
                                     style='padding:4px; font-size:70%'),
                       "Start Day",
                       actionButton("tab2_datemin_right",
                                    icon("plus"),
                                    style='padding:4px; font-size:70%')),
              fluidRow(class = "myDateRow",
                       actionButton("tab2_timemin_left",
                                    icon("minus"),
                                    style='padding:4px; font-size:70%'),
                       "Start Time",
                       actionButton("tab2_timemin_right",
                                    icon("plus"),
                                    style='padding:4px; font-size:70%')),
              fluidRow(class = "myDateRow",
                       actionButton("tab2_datemax_left",
                                    icon("minus"),
                                    style='padding:4px; font-size:70%'),
                       "End Day",
                       actionButton("tab2_datemax_right",
                                    icon("plus"),
                                    style='padding:4px; font-size:70%')),
              fluidRow(class = "myDateRow",
                       actionButton("tab2_timemax_left",
                                    icon("minus"),
                                    style='padding:4px; font-size:70%'),
                       "End Time",
                       actionButton("tab2_timemax_right",
                                    icon("plus"),
                                    style='padding:4px; font-size:70%')),
              selectInput("tab2_selected_output", label = "Outputs to plot",
                          choices = names(x),
                          multiple = TRUE,
                          selected = first(names(x))),
              selectInput("tab2_var", label = "Variable",
                          choices = var_with_dim(x[[1]],"time"),
                          selected = "Qg"),
              checkboxInput("tab2_subset", "subset data", value = FALSE),
              uiOutput(outputId = 'tab2_dynamic_nsoil'),
              uiOutput(outputId = 'tab2_dynamic_nair'),
              uiOutput(outputId = 'tab2_dynamic_nspecies'),
              uiOutput(outputId = 'tab2_dynamic_nveg'),
              uiOutput(outputId = 'tab2_dynamic_nleafage'),
              selectInput("tab2_type", label = "Plot type",
                          choices = c("standard",
                                      "heatmap",
                                      "daily_heatmap"),
                          selected = "standard"),
              selectInput("tab2_facet", label = "facet",
                          choices = c("models",
                                      get_dim_info(x[[1]])$dimname),
                          multiple = TRUE,
                          selected = NULL),
              conditionalPanel(
                condition = "input.tab2_type == 'standard'",
                selectizeInput("tab2_color", label = "color",
                               choices = c("models",
                                           get_dim_info(x[[1]])$dimname),
                               options = list(
                                 placeholder = 'Empty',
                                 onInitialize = I('function() { this.setValue(""); }')))
              ),
              conditionalPanel(
                condition = "input.tab2_type == 'standard'",
                selectizeInput("tab2_linetype", label = "linetype",
                               choices = c("models",
                                           get_dim_info(x[[1]])$dimname),
                               options = list(
                                 placeholder = 'Empty',
                                 onInitialize = I('function() { this.setValue(""); }')))
              ),
              fluidRow(
                actionButton("tab2_UpdateView", icon("refresh")),
                downloadButton('tab2_download', "Download Plot")),
              width = 2
            ),
            ## Output ---------------------------------------------------
            mainPanel(
              plotOutput("tab2_plot"),
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
                          }")),
                tags$style(HTML(".myDateRow{
                                font-size: 8px;
                                line-height: 10px;
                                min-height: 10px;}")))
    )
  )
}



