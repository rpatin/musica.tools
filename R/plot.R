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
##' geom_raster scale_y_continuous scale_x_continuous scale_y_reverse
##' @importFrom stringr str_wrap
##' @importFrom scales breaks_pretty
##' @importFrom dplyr last mutate ungroup group_by select 
##' @importFrom tidyr pivot_longer
##' @importFrom lubridate hour minute second
##' @export
##' 
##' 

ggplot_variable <- function(df,
                            time_range,
                            format.date = "%b %Y",
                            out.type = c("standard", "heatmap", "daily_heatmap"),
                            x, y, color, linetype, facet_formula) {
  # df <- list_value$relative_height
  args <- .check_ggplot_variable(df = df,
                                 time_range = time_range,
                                 format.date = format.date,
                                 out.type = out.type,
                                 x = x, y = y,
                                 color = color, linetype = linetype,
                                 facet_formula = facet_formula)
  for (argi in names(args)) { 
    assign(x = argi, value = args[[argi]]) 
  }
  rm(args)
  
  this_ylab <- attr_legend(df)
  skip_ylab <- FALSE
  this_variable <- attr(df, 'var')
  
  if (out.type == "standard") {
    g <- 
      ggplot(df) 
    
    if (!is.null(color) & !is.null(linetype)) {
      g <- g + geom_line(aes(x = .data[[x]],
                             y = .data[[y]],
                             color = .data[[color]],
                             linetype = .data[[linetype]]))
    } else if (!is.null(color)) {
      g <- g + geom_line(aes(x = .data[[x]],
                             y = .data[[y]],
                             color = .data[[color]]))
      
    } else if (!is.null(linetype)) {
      g <- g + geom_line(aes(x = .data[[x]],
                             y = .data[[y]]))
    } else {
      g <- g + geom_line(aes(x = .data[[x]],
                             y = .data[[y]],
                             linetype = .data[[linetype]]))
    }
    if (!is.null(facet_formula)) {
      g <- 
        g +
        facet_wrap(formula(facet_formula), ncol = 1)
    }
    g <- g +
      scale_x_datetime(NULL, breaks = breaks_pretty(10))
  } else if (out.type == "daily_heatmap") {
    skip_ylab <- TRUE
    df_timed <- 
      df %>% 
      mutate(date = as.Date(time),
             hour = hour(time) + minute(time)/60 + second(time)/3600,
             datenum = as.numeric(date)*24*3600,
             datepos = as.POSIXct(datenum, origin = "1970-01-01"), 
             year = year(datepos), 
             month = month(datepos), 
             day = day(datepos))
    
    df_breaks <- 
      df_timed %>% 
      filter(day == 1) %>% 
      group_by(year, month) %>% 
      filter(time == min(time)) %>% 
      ungroup() %>% 
      mutate(label = format(time, format.date)) %>% 
      select(time, label)
    
    
    g <-
      ggplot(df_timed) +
      geom_raster(aes(y = datenum, x = hour, 
                      fill = .data[[this_variable]])) +
      scale_x_continuous(breaks = breaks_pretty(n = 13), expand = c(0,0)) +
      scale_y_reverse(labels = df_breaks$label,
                      breaks = as.numeric(df_breaks$time)) +
      scale_fill_viridis_c(str_wrap(this_ylab, width = 15),
                           option = "D", direction = 1) +
      xlab("Time of the day (h)") +
      ylab(NULL)

    if (!is.null(facet_formula)) {
      g <- 
        g +
        facet_wrap(formula(facet_formula), nrow = 1)
    }
  } else if (out.type == "heatmap") {
    skip_ylab <- TRUE
    g <-
      ggplot(df) +
      geom_raster(aes(x = time,
                      y = .data[[y]],
                      fill = .data[[this_variable]])) +
      scale_y_reverse(breaks = breaks_pretty(n = 10)) +
      scale_fill_viridis_c(str_wrap(this_ylab, width = 15),
                           option = "D", direction = -1) +
      # geom_hline(yintercept = seq(0.5, 10.5), color = "darkgreen", linetype = 2) + # not nice
      ylab(y) +
      xlab(NULL)
    
    if (!is.null(facet_formula)) {
      g <- 
        g +
        facet_wrap(formula(facet_formula), ncol = 1)
    }
  }
  if (!is.null(color) && color == "nspecies") {
    g <- g +
      scale_color_brewer("Species", palette = "Set1") 
  }
  if ( !skip_ylab ) {
    
    g <- g + ylab(str_wrap(this_ylab, width = 25))
  }
  g
}

.check_ggplot_variable <- function(df, time_range, format.date, out.type,
                                   x, y, color, linetype, facet_formula) {
  
  
  # input data.frame --------------------------------------------------------
  
  this_variable <- attr(df, 'var')
  
  if (!is.null(attr(df,'models'))) {
    attr.list <- attributes(df)
    df <- pivot_longer(df, !attr(df,'dimname'),
                       names_to = "models",
                       values_to = this_variable)
    for (this.attr in names(attr.list)[-c(1:3)]) {
      attr(df, this.attr) <- attr.list[[this.attr]]
    }
  }
  
  
  # time-range --------------------------------------------------------------
  
  if (!missing(time_range)) {
    df <-
      df %>% 
      filter(time >= time_range[1],
             time <= time_range[2])
  }
  
  # out.type ----------------------------------------------------------------
  
  if (length(out.type) > 1) {
    out.type <- first(out.type)
  }
  
  
  # standard ------------------------------------------------------------
  if (out.type == "standard") {
    list.var <- colnames(df)
    
    if (missing(x)) {
      x <- "time"
    } else {
      .fun_testIfIn(x, attr(df, "dimname"))
    }
    list.var <- list.var[-which(list.var == x)]
    
    if (missing(y)) {
      y <- attr(df, "var") 
    } else {
      .fun_testIfIn(y, attr(df, "dimname"))
    }
    list.var <- list.var[-which(list.var == y)]
    
    if (missing(color)) {
      if (!is.null(attr(df, "models"))) {
        color <- "models"
        list.var <- list.var[-which(list.var == color)]
      } else if (length(list.var) > 0) {
        color <- first(list.var)
        list.var <- list.var[-which(list.var == color)]
      } else {
        color <- NULL
      }
    } else if (!is.null(color)) {
      .fun_testIfIn(color, attr(df, "dimname"))
      which.var <- which(list.var == color)
      if (length(which.var) > 0) list.var <- list.var[-which.var]
    }
    
    if (missing(linetype)) {
      if (length(list.var) > 0) {
        linetype <- first(list.var)
        list.var <- list.var[-which(list.var == linetype)]
      } else {
        linetype <- NULL
      }
    } else if (!is.null(linetype)) {
      .fun_testIfIn(linetype, attr(df, "dimname"))
      which.var <- which(list.var == linetype)
      if (length(which.var) > 0) list.var <- list.var[-which.var]
    }
    
    if (missing(facet_formula)) {
      if (length(list.var) > 0) {
        facet_formula <- paste0("~", paste0(list.var, collapse = "+"))
      } else {
        facet_formula <- NULL
      }
    } 
  }
  
  
  # daily_heatmap -----------------------------------------------------------
  if (out.type == "daily_heatmap") {
    list.var <- colnames(df)
    .fun_testIfIn("time", attr(df, "dimname"))
    list.var <- list.var[-which(list.var == "time")]
    list.var <- list.var[-which(list.var == attr(df, 'var'))]
    if (missing(facet_formula)) {
      if (length(list.var) > 0) {
        facet_formula <- paste0("~", paste0(list.var, collapse = "+"))
      } else {
        facet_formula <- NULL
        
      }
    } 
    x <- NULL
    y <- NULL
    linetype <- NULL
    color <- NULL
  } 
  
  # heatmap -----------------------------------------------------------
  if (out.type == "heatmap") {
    list.var <- colnames(df)
    .fun_testIfIn("time", attr(df, "dimname"))
    list.var <- list.var[-which(list.var == "time")]
    list.var <- list.var[-which(list.var == attr(df, 'var'))]
    
    if (missing(y)) {
      y <- first(list.var)
    } else {
      .fun_testIfIn(y, attr(df, "dimname"))
    }
    list.var <- list.var[-which(list.var == y)]
    
    if (missing(facet_formula)) {
      if (length(list.var) > 0) {
        facet_formula <- paste0("~", paste0(list.var, collapse = "+"))
      } else {
        facet_formula <- NULL
      }
    }
    x <- "time"
    linetype <- NULL
    color <- NULL
  } 
  
  list(df = df,
       format.date = format.date,
       out.type = out.type,
       x = x,
       y = y,
       color = color,
       linetype = linetype,
       facet_formula = facet_formula)
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
