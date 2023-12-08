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
##' guide_legend guides element_rect scale_fill_viridis_c scale_y_reverse
##' geom_raster scale_y_continuous
##' @importFrom stringr str_wrap
##' @importFrom scales breaks_pretty
##' @importFrom dplyr last mutate
##' @importFrom lubridate hour minute second
##' @export
##' 
##' 

ggplot_variable <- function(df, heatmap = TRUE, daily_heatmap = TRUE, 
                            time_range) {
  # df <- list_value$relative_height
  this_variable <- last(colnames(df))
  this_ylab <- attr_legend(df)
  skip_ylab <- FALSE
  
  stopifnot(is.logical(heatmap))
  stopifnot(is.logical(daily_heatmap))
  if (daily_heatmap & ncol(df) > 3) {
    stop("daily heatmap can only be produced for 1 dimension variables")
  }
  
  if (!missing(time_range)) {
    df <-
      df %>% 
      filter(time >= time_range[1],
             time <= time_range[2])
  }
  
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
    if(!daily_heatmap) {
      g <- 
        ggplot(df) +
        geom_line(aes(x = time, y = .data[[this_variable]])) +
        xlab(NULL)
    } else {
      skip_ylab <- TRUE
      df_timed <- 
        df %>% 
        mutate(date = as.Date(time)) %>% 
        mutate(hour = hour(time) + minute(time)/60 + second(time)/3600)
      
      g <-
        ggplot(df_timed) +
        geom_raster(aes(x = date, y = hour, fill = .data[[this_variable]])) +
        scale_y_continuous(breaks = breaks_pretty(n = 24), expand = c(0,0)) +
        scale_fill_viridis_c(str_wrap(this_ylab, width = 15),
                             option = "D", direction = 1) +
        ylab("Time of the day") +
        xlab(NULL)
      
    }
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
    if (!heatmap) {
      g <-
        ggplot(df) +
        geom_line(aes(x = time, y = .data[[this_variable]],
                      color = factor(nsoil))) +
        scale_color_viridis_d("Soil Layer", option = "C", direction = -1) +
        guides(color = guide_legend(ncol = 2)) +
        xlab(NULL) +
        theme_dark() +
        theme(plot.background = element_rect(fill = "grey80"),
              legend.background =  element_rect(fill = "grey80"))
    } else {
      skip_ylab <- TRUE
      g <-
        ggplot(df) +
        geom_raster(aes(x = time, y = nsoil, fill = .data[[this_variable]])) +
        scale_y_reverse(breaks = breaks_pretty(n = 10)) +
        scale_fill_viridis_c(str_wrap(this_ylab, width = 15),
                             option = "D", direction = -1) +
        # geom_hline(yintercept = seq(0.5, 10.5), color = "darkgreen", linetype = 2) + # not nice
        ylab("Soil Layer") +
        xlab(NULL)
    }
  }
  
  if ( !skip_ylab ) {
    
    g <- g + ylab(str_wrap(this_ylab, width = 25))
  }
  g
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

ggplot_list_var <- function(x, list_var, time_range, daily_heatmap = TRUE) {
  list_plot <- list()
  for (this_var in list_var) {
    df.try <- try({ 
      df <- get_variable(x, this_var, time_range = time_range)
      this_ggplot <- 
        ggplot_variable(df, daily_heatmap = daily_heatmap) 
    })
    if (!inherits(df.try, "try-error")) {
      list_plot[[this_var]] <- this_ggplot
    }
  }
  plot_grid(plotlist = list_plot, ncol = 1, align = "v", axis = "lr")
}



# dygraph_variable ---------------------------------------------------------
##' @name dygraph_variable
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
##' @importFrom dygraphs dygraph dyAxis dyHighlight dyLegend
##'  dyRangeSelector dyOptions
##' @importFrom xts xts
##' @importFrom tidyr pivot_wider
##' @importFrom dplyr last
##' @importFrom scales viridis_pal brewer_pal
##' @export
##' 
##' 

dygraph_variable <- function(df,
                             pixheight = 150, pixwidth = 1500,
                             axisLabelWidth = 75,
                             group = "Overview",
                             time_range) {
  
  if (!missing(time_range)) {
    df <-
      df %>% 
      filter(time >= time_range[1],
             time <= time_range[2])
  }
  
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
  this_dygraph <-
    dygraph(this.xts, 
            group = group,
            height = pixheight,
            width = pixwidth) %>% 
    dyAxis("y", label = this_ylab,
           axisLabelWidth = axisLabelWidth) %>%
    dyLegend(hideOnMouseOut = TRUE,
             show = "onmouseover")
  
  if (set_hover) {
    if (any(grepl("nsoil", colnames(this.xts)))) {
      this_dygraph <- 
        this_dygraph %>% 
        dyOptions(colors = rev(viridis_pal()(ncol(this.xts))))
    } else {
      this_dygraph <- 
        this_dygraph %>% 
        dyOptions(colors = brewer_pal(palette = "Set2")(ncol(this.xts)))
    }
    this_dygraph <- 
      this_dygraph %>% 
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),
                  hideOnMouseOut = TRUE)
  }
  this_dygraph
}


# dygraph_comparison ---------------------------------------------------------
##' @name dygraph_comparison
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
##' @importFrom dygraphs dygraph dyAxis dyHighlight dyLegend
##'  dyRangeSelector dyOptions
##' @importFrom xts xts
##' @importFrom tidyr pivot_wider
##' @importFrom dplyr last
##' @importFrom scales viridis_pal brewer_pal
##' @export
##' 
##' 

dygraph_comparison <- function(df,
                               pixheight = 150, pixwidth = 1500,
                               axisLabelWidth = 75,
                               group = "Overview",
                               time_range,
                               main.title = NULL) {
  
  if (!missing(time_range)) {
    df <-
      df %>% 
      filter(time >= time_range[1],
             time <= time_range[2])
  }
  
  this_variable <- attr(df, "var")
  this_ylab <- attr_legend(df)
  ndim <- attr(df,"ndim")
  set_hover <- TRUE
  if (ndim == 1) {
    df.in <- list(df)
  } else if (ndim == 2) {
    df.in <- split(df, factor(as.data.frame(df)[,1]))
  } else {
    stop("unsupported dimension number")
  }
  
  this.main <- main.title
  dygraph.list <- lapply(df.in, function(this.df){
    if (ndim == 1) {
      this.xts <- xts(this.df[, -which(colnames(this.df) == "time")],
                      order.by = this.df$time)
    } else {
      if (is.null(main.title)) {
        this.main = paste0(first(colnames(this.df)), " = ", first(this.df[,1]))
      }
      this.df <- this.df[,-1]
      this.xts <- xts(this.df[, -which(colnames(this.df) == "time")],
                      order.by = this.df$time)
    }
    this_dygraph <-
      dygraph(this.xts, 
              group = group,
              height = pixheight,
              width = pixwidth, 
              main = this.main) %>% 
      dyAxis("y", label = this_ylab,
             axisLabelWidth = axisLabelWidth) %>%
      dyLegend(hideOnMouseOut = TRUE,
               show = "onmouseover")
    
    this_dygraph <- 
      this_dygraph %>% 
      dyOptions(colors = brewer_pal(palette = "Set2")(ncol(this.xts)))
    
    this_dygraph <- 
      this_dygraph %>% 
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2,
                                             strokePattern = "dashed"),
                  hideOnMouseOut = TRUE)
    this_dygraph
  })
  
  if (ndim == 1 || length(dygraph.list) == 1) {
    return(dygraph.list[[1]])
  } else {
    return(dygraph.list)
  }
}
