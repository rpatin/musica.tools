# summary_markdown ---------------------------------------------------------
##' @name summary_markdown
##' @author Remi Lemaire-Patin
##' 
##' @title render a markdown report
##' 
##' @description This function render a markdown report for a selection of 
##' variables. Report includes interactive time-series plot using \code{dygraphs}
##' package for the selection of variables.
##' 
##'
##' @param x a \code{ncdf4} object
##' @param filename a \code{character}, file name to save the report, without
##'   extension (automatically added)
##' @param template \emph{(optional)}, a \code{character}, with preset of
##'   variables that can be exported, see Details.
##' @param list_var \emph{(optional, ignored if \code{template} argument was
##'   given)} a \code{character vector} with list of variables to be exported.
##' @param add_study_site (\emph{optional}, default \code{FALSE}) a
##'   \code{boolean}, if \code{TRUE} adds a leaflet with location of study site
##' @param time_range (\emph{optional}) a \code{POSIXct vector} with two values:
##'   the time range to extract
##' @param n.soil.level (default \code{5}), a \code{numeric}, number of soil
##'   level to be kept
##' @param n.air.level (default \code{5}), a \code{numeric}, number of air level
##'   to be kept
##' @param out.dir (default \code{'./'}), a \code{character}, output directory
##'   to store results
##' @param obs (default \code{NULL}), a \code{data.frame}, with corresponding
##'   observations
##' @param summary.title (default \code{"MuSICA Output Summary"}), a
##'   \code{character}, used as title for markdown summary
##' @param ... additional parameters transmitted to \code{\link{dygraph_variable}}
##' 
##' @return NULL
##' 
##' @family summary
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' summary_markdown(x, "test_summary", template = "Overview")
##' file.remove("test_summary.html")
##' @importFrom cli cli_alert_warning
##' @importFrom dplyr first
##' @importFrom htmltools tagList
##' @export
##' 
##' 

summary_markdown <- function(x, filename, template, list_var, 
                             add_study_site = FALSE, time_range = NULL, 
                             n.soil.level = 5, n.air.level = 5,
                             out.dir = "./", obs = NULL, 
                             summary.title = "MuSICA Output Summary",
                             ...) {
  args <- .check_summary(x = x, 
                         type = "markdown",
                         filename = filename,
                         template = template,
                         list_var = list_var,
                         add_study_site = add_study_site, 
                         time_range = time_range, 
                         ...)
  for (argi in names(args)) { 
    assign(x = argi, value = args[[argi]]) 
  }
  rm(args)
  
  if (inherits(x, "ncdf4")) {
    if (is.null(obs)) {
      markdown.file <- "template.Rmd"
    } else {
      markdown.file <- "template_obs.Rmd"
    }
  } else {
    markdown.file <- "template_cmp.Rmd"
  }
  if (requireNamespace("rmarkdown")) {
    rmarkdown::render(
      input = system.file(package = "musica.tools",
                          paste0("/rmarkdown/", markdown.file)),
      output_file = paste0(filename,".html"),
      output_dir = out.dir,
      encoding     = 'UTF-8'
    )
  }
}


# summary_pdf ---------------------------------------------------------
##' @name summary_pdf
##' @author Remi Lemaire-Patin
##' 
##' @title render a pdf report
##' 
##' @description This function render a markdown report for a selection of 
##' variables. Report includes static plot for a given selection of variable
##' @param file.width file width transmitted to \code{pdf()}
##' @param file.height file height transmitted to \code{pdf()}
##' @param ... additional parameters transmitted to \code{\link{ggplot_variable}}
##' 
##' @return NULL
##' 
##' @family summary
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' summary_pdf(x, "test_summary", template = "Overview")
##' file.remove("test_summary.pdf")
##' 
##' @inheritParams summary_markdown
##' @importFrom cli cli_alert_warning
##' @importFrom gridExtra grid.arrange
##' @importFrom grDevices cm pdf dev.off
##' @export
##' 
##' 

summary_pdf <- function(x, filename, template, list_var, add_study_site = FALSE, 
                        time_range, file.width, file.height, ...) {
  args <- .check_summary(x = x, 
                         type = "pdf",
                         filename = filename,
                         template = template,
                         list_var = list_var,
                         add_study_site = add_study_site,
                         time_range = time_range,
                         ...)
  for (argi in names(args)) { 
    assign(x = argi, value = args[[argi]]) 
  }
  rm(args)
  
  pdf(file = paste0(filename,".pdf"),
      width = file.width,
      height = file.height)
  grid.arrange(
    ggplot_list_var(x, list_var, time_range)
  )
  dev.off()
}


# .check_summary ----------------------------------------------------------


.check_summary <- function(x, filename, template, list_var,
                           add_study_site, type, time_range, ...) {
  
  stopifnot(inherits(x, "ncdf4") | all(sapply(x, function(dx) inherits(dx, "ncdf4"))))
  .fun_testIfIn(type, c("markdown","pdf"))
  
  if (missing(filename)) {
    stop("Missing `filename`. Please provide a proper `filename` without extensions")
  } else if (!file.access(filename, mode = 2)) {
    stop("Invalid path provided, `filename` could not be written")
  }
  if (inherits(x, "ncdf4")) {
    list_var_potential <-  names(x$var)
  } else {
    list_var_potential <- unique(unlist(sapply(x, function(dx) names(dx$var))))
  }
  
  if (!missing(template)) {
    .fun_testIfIn(template, c("Overview","Heat flux","Water",
                              "Soil", "Plants", "Isotope","Ox18","Deut"))
    if (!missing(list_var)) {
      cli_alert_warning("`list_var` argument will be ignored as `template` was provided")
    }
    
    list_var <-   switch(
      template,
      "Overview"  = c("NEE","Qle","Qh","Qg","w_soil"),
      "Heat flux" = c("Rnet", "Qle","Qh","Qg"),
      "Water"     = c("runoff","q_h2o_soil_liq","q_h2o_soil_vap", "transpir",
                      "w_soil","h_soil","Evap"),
      "Soil"      = c("w_soil","h_soil", "dt_soil", "Qco2_soil", "Qco2_soil_SS", "Qg"),
      "Plants"    = c("h_xylem","gpp","transpir","fh_xylem",
                      "w_soil","h_soil","root_uptake"), 
      "Isotope"   = c("d_w_xylem_ox18","d_w_xylem_deut",
                      "d_w_soil_ox18","d_w_soil_deut",
                      "d_w_inleaf_sunlit_dry_ox18","d_w_inleaf_sunlit_dry_deut",
                      "d_w_inleaf_shaded_dry_ox18","d_w_inleaf_shaded_dry_ox18",
                      "d_vapour_ox18_ref","d_vapour_deut_ref"), 
      "Ox18"   = c("d_w_xylem_ox18",
                   "d_w_soil_ox18",
                   "d_w_inleaf_sunlit_dry_ox18",
                   "d_w_inleaf_shaded_dry_ox18",
                   "d_vapour_ox18_ref"), 
      "Deut"   = c("d_w_xylem_deut",
                   "d_w_soil_deut",
                   "d_w_inleaf_sunlit_dry_deut",
                   "d_w_inleaf_shaded_dry_deut",
                   "d_vapour_deut_ref")
    )
    
    list_var <- list_var[list_var %in% list_var_potential]
    
  } else if ( missing(list_var)) {
    stop("`list_var` required when `template is not provided`")
  } else {
    .fun_testIfIn(list_var, list_var_potential)
  }
  stopifnot(inherits(add_study_site, "logical"))
  
  if (missing(time_range)) {
    time_range <- NULL
  } else {
    stopifnot(inherits(time_range,"POSIXct"))
    stopifnot(length(time_range) == 2)
  }
  dot.args <- list(...)
  
  ## Markdown/dygraph specific argument ------------------------------------------
  if (type == "markdown") {
    if (!"pixheight" %in% names(dot.args)) {
      pixheight <- 145
    } else {
      pixheight <- dot.args$pixheight
    }
    if (!"pixwidth" %in% names(dot.args)) {
      pixwidth <- 1000
    } else {
      pixwidth <- dot.args$pixwidth
    }
    if (!"axisLabelWidth" %in% names(dot.args)) {
      axisLabelWidth <- 75
    } else {
      axisLabelWidth <- dot.args$axisLabelWidth
    }
    return(
      list(x = x, 
           filename = filename,
           list_var = list_var,
           add_study_site = add_study_site,
           time_range = time_range,
           pixwidth = pixwidth,
           pixheight = pixheight,
           axisLabelWidth = axisLabelWidth)
    )
  } else if (type == "pdf") {
    ## pdf/ggplot specific argument ------------------------------------------
    if (!"file.width" %in% names(dot.args)) {
      file.width <- 29.7/cm(1)
    } else {
      file.width <- dot.args$file.width
    }
    if (!"file.height" %in% names(dot.args)) {
      file.height <- 21/cm(1)
    } else {
      file.height <- dot.args$file.height
    }
    return(
      list(x = x, 
           filename = filename,
           list_var = list_var,
           add_study_site = add_study_site,
           time_range = time_range,
           file.width = file.width,
           file.height = file.height)
    )
  }
}
