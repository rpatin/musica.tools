# ggplot_variable ---------------------------------------------------------
##' @name ggplot_variable
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
##' 
##' @importFrom ggplot2 ggplot aes geom_line geom_step xlab ylab facet_wrap 
##' scale_color_brewer scale_color_viridis_d theme element_blank theme_dark
##' guide_legend guides
##' @importFrom stringr str_wrap
##' @export
##' 
##' 

ggplot_variable <- function(df) {
  # df <- list_value$relative_height
  this_variable <- last(colnames(df))
  this_ylab <- attr_legend(df)
  
  
  ## relative_height (nair) ---------------------------------------------------------
  if (this_variable %in% c("relative_height")) {
    df <- rbind(
      data.frame(nair = 0, relative_height = 0),
      df,
      data.frame(nair = Inf, relative_height = max(df$relative_height))
    )
    g <- 
      ggplot(df) +
      geom_step(aes(x = nair, y = relative_height))
  }
  
  ## longitude & latitude ---------------------------------------------------------
  if (this_variable %in% c("longitude","latitude")) {
    return(df[,this_variable])
  }
  
  ## variable with [time] ------------------------------------------------------
  if (this_variable %in% c("veget_height_top", "Qle", "Qh", "NEE",
                           "Qg", "runoff")) {
    g <- 
      ggplot(df) +
      geom_line(aes(x = time, y = .data[[this_variable]])) +
      xlab(NULL)
  }
  
  ## variable with [nspecies,time]   -------------------------------------------
  if (this_variable %in% c("h_canopy", "gpp", "transpir", "d_w_xylem_ox18",
                           "d_w_xylem_deut", "fh_xylem")) {
    g <-
      ggplot(df) +
      geom_line(aes(x = time, y = .data[[this_variable]],
                    color = factor(nspecies))) +
      # facet_wrap(~nspecies, ncol = 1) +
      scale_color_brewer("Species", palette = "Set1") +
      xlab(NULL) +
      theme(strip.text = element_blank())
  }
  
  ## variable with [nsoil,time] -----------------------------------------------
  if (this_variable %in% c("w_soil", "h_soil", "d_w_soil_ox18", "d_w_soil_deut",
                           "q_h2o_soil_liq", "q_h2o_soil_vap")) {
    g <- 
      ggplot(df) +
      geom_line(aes(x = time, y = .data[[this_variable]],
                    color = factor(nsoil))) +
      scale_color_viridis_d("Soil Layer", option = "C", direction = -1) +
      guides(color = guide_legend(ncol=2)) +
      xlab(NULL) +
      theme_dark() +
      theme(plot.background = element_rect(fill = "grey80"),
            legend.background =  element_rect(fill = "grey80"))
  }
  g + ylab(str_wrap(this_ylab, width = 30))
}


# ggplot_list_var ---------------------------------------------------------
##' @name ggplot_list_var
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
##' 
##' @importFrom cowplot plot_grid
##' @export
##' 
##' 

ggplot_list_var <- function(x, list_var, time_range) {
  list_plot <- list()
  for (this_var in list_var) {
    df <- get_variable(x, this_var, time_range = time_range)
    this_ggplot <- 
      ggplot_variable(df)
    list_plot[[this_var]] <- this_ggplot
  }
  plot_grid(plotlist = list_plot, ncol = 1, align = "v", axis = "lr")
}



# dygraph_var ---------------------------------------------------------
##' @name dygraph_var
##' @author Remi Lemaire-Patin
##' 
##' @title produce \code{dygraph} for a single variable
##' 
##' @description This function plot the output of \code{\link{get_variable}}
##' with an interactive dygraph app
##' 
##' 
##' @param df a \code{data.frame} object
##' @param pixheight height of the \code{dygraph} object in pixel
##' @param pixwidth width of the \code{dygraph} object in pixel
##' @param axisLabelWidth height of the y axis label
##' @return
##' 
##' A \code{dygraph} object
##' 
##' @family plot
##'   
##' @examples
##' library(ncdf4)
##' 
##' 
##' @importFrom dygraphs dygraph dyAxis dyHighlight dyLegend dyRangeSelector
##' @importFrom xts xts
##' @importFrom tidyr pivot_wider
##' @export
##' 
##' 

dygraph_variable <- function(df,
                             pixheight = 150, pixwidth = 1500,
                             axisLabelWidth = 75,
                             group = "Overview") {
  this_variable <- last(colnames(df))
  this_ylab <- attr_legend(df)
  set_hover <- FALSE
  if (ncol(df) > 2) {
    if (ncol(df) == 3) {
      set_hover <- TRUE
      df <- pivot_wider(df,
                        names_from = colnames(df)[1],
                        names_prefix = paste0(colnames(df)[1],"_"),
                        values_from = last(colnames(df)))
    } else {
      stop("unsupported dimension number")
    }
  }
  this.xts <- xts(df[, -which(colnames(df) == "time")],
                  order.by = df$time)
  this_dygraph <- dygraph(this.xts, 
          group = group,
          height = pixheight,
          width = pixwidth) %>% 
    dyAxis("y", label = this_ylab,
           axisLabelWidth = axisLabelWidth) %>%
    dyLegend(hideOnMouseOut = TRUE,
             show = "onmouseover")
  if (set_hover) {
    this_dygraph <- 
      this_dygraph %>% 
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
                  hideOnMouseOut = TRUE)
  }
  this_dygraph
}

