# ggplot_variable ---------------------------------------------------------
##' @name ggplot_variable
##' @author Remi Lemaire-Patin
##' 
##' @title plot MuSICA model output
##' 
##' @description This function is a wrapper around \code{ggplot} to plot the
##'   output of a MuSICA model or a comparison of model. It can plot the output
##'   of \code{\link{get_variable}}, \code{\link{get_variable_comparison}} or
##'   \code{\link{get_two_variables}}. See details for the list of graphics
##'   available and their options.
##' 
##' @inheritParams get_variable
##' @param df a \code{data.frame} object, output of \code{\link{get_variable}},
##'   \code{\link{get_variable_comparison}} or  \code{\link{get_two_variables}}.
##' @param out.type a \code{character}, type of plot to be returned. See
##'   details.
##' @param x (\emph{optional}), x aesthetics used in the plot (see details).
##' @param y (\emph{optional}), y aesthetics used in the plot (see details).
##' @param color (\emph{optional}), color aesthetics used in the plot (see
##'   details).
##' @param linetype (\emph{optional}), linetype aesthetics used in the plot (see
##'   details).
##' @param shape (\emph{optional}), shape aesthetics used in the plot (see
##'   details).
##' @param fill (\emph{optional}), fill aesthetics used in the plot (see
##'   details).
##' @param facet_formula (\emph{optional}), facet formula (or vector of
##'   variables) used in the plot, available for all values of \code{out.type}.
##' @param xrange (\emph{optional}), \code{numeric} vector, used to restrict the
##'   range of x values on the plot.
##' @param yrange (\emph{optional}), \code{numeric} vector, used to restrict the
##'   range of y values on the plot.
##' @param fillrange (\emph{optional}), \code{numeric} vector, used to subset
##'   the variable used as fill aesthetics in the plot.
##' @param bin2d (\emph{optional}, default \code{TRUE}), a \code{boolean}, if
##'   \code{TRUE}, the code uses \code{geom_bin2d} instead of default
##'   \code{geom_point}.
##' @param diffmodels  (\emph{optional}, default \code{FALSE}), a
##'   \code{boolean}, if \code{TRUE}, indicates that \code{df} contains the
##'   difference between two models. Plot is then adjusted accordingly
##' @param nrow.facet (\emph{optional}), a \code{numeric}, number of facet rows
##'   when argument \code{facet_formula} was provided.
##' @param format.date (\emph{optional}, default \code{'\%b \%Y'})  a
##'   \code{character} interpreted as a date format, used  to format date when
##'   \code{out.type = 'daily_heatmap'}.
##' @return
##' 
##' A \code{ggplot} object
##' 
##' @details
##' The following \code{out.type} values are available:
##' \itemize{
##'   \item \code{out.type = "standard"}.
##'     \itemize{
##'       \item Standard time-series with \code{geom_line}.
##'       \item Available additional aesthetic argument: \code{color},
##'       \code{linetype}.
##'       \item Available subsetting argument: \code{yrange}.
##'     }
##'   \item \code{out.type = "heatmap"}.
##'     \itemize{
##'       \item Heatmap plot made with \code{geom_raster}. \code{x} is set to
##'       \code{time} ; \code{y} is set to one of \code{nsoil}, \code{nair} or
##'       \code{nveg} ; \code{fill} is set to the current variable.
##'       \item No additional aesthetic argument available.
##'       \item Available subsetting argument:  \code{yrange}, \code{fillrange}.
##'     }
##'   \item \code{out.type = "daily_heatmap"}.
##'     \itemize{
##'       \item Heatmap plot made with \code{geom_raster} to highlight daily
##'       variations. \code{x} is set to daily time (hours) ; \code{y} is set to
##'       day of the year ; \code{fill} is set to the current variable.
##'       \item No additional aesthetic argument available.
##'       \item Available subsetting argument: \code{fillrange}.
##'     }
##'   \item \code{out.type = "boxplot"}.
##'     \itemize{
##'       \item Boxplot made with \code{geom_boxplot}.\code{y} is set to the
##'       current variable.
##'       \item Available additional aesthetic argument: \code{x}, \code{fill}.
##'       \item Available subsetting argument: \code{yrange}.
##'     }
##'   \item \code{out.type = "density"}.
##'     \itemize{
##'       \item Density plot made with \code{geom_density}.\code{x} is set to
##'       the current variable, \code{y} is set to the density.
##'       \item Available additional aesthetic argument: \code{color},
##'       \code{linetype}.
##'       \item Available subsetting argument: \code{xrange}, \code{yrange}.
##'     }
##'   \item \code{out.type = "histogram"}.
##'     \itemize{
##'       \item Histogram plot made with \code{geom_histogram}.\code{x} is set
##'       to the current variable, \code{y} is set to the density.
##'       \item Available additional aesthetic argument: \code{fill}.
##'       \item Available subsetting argument: \code{xrange}, \code{yrange}.
##'     }
##'   \item \code{out.type = "scatterplot"}.
##'     \itemize{
##'       \item Scatterplot made with \code{geom_point} and representing one
##'       variables against a second variable, or one model against a second
##'       model. \code{x} is set to the first variable or model, \code{y} is set
##'       to the second variable or model. 
##'       \item Available additional aesthetic argument: \code{color},
##'       \code{shape}.
##'       \item Available subsetting argument: \code{xrange}, \code{yrange}.
##'     }
##'   }
##' The following additional argument can be given:
##'   \itemize{
##'   \item x: column name available in \code{df} for x aesthetics. Available
##'   for \code{out.type = 'boxplot'}
##'   \item y: column name available in \code{df} for y aesthetics. Available
##'   for \code{out.type = 'heatmap'}, but must be \code{'nsoil'}, \code{'nveg'}
##'   or \code{'nair'}.
##'   \item color: column name available in \code{df} to
##'   separate data (points or lines) by color. Available for \code{out.type =
##'   'standard'}, \code{'density'}, or \code{'scatterplot'}. 
##'   \item fill: column name available in \code{df} to separate data by fill
##'   color Available for \code{out.type = 'boxplot'} or \code{'histogram'}.
##'   \item shape: column name available in \code{df} to separate points by
##'   shape. Available for \code{'scatterplot'}.
##'   \item linetype: column name available in \code{df} to separate lines by
##'   linetype. Available for \code{out.type = 'standard'} or \code{'density'}.
##'   }
##' The following additional subsetting arguments can be given:
##'   \itemize{
##'     \item xrange: vector with min and max to apply on x axis. Available for
##'     \code{out.type = 'scatterplot'}, \code{'histogram'} or \code{'density'}.
##'     \item yrange: vector with min and max to apply on y axis. Available for
##'     \code{out.type = 'standard'}, \code{'heatmap'}, \code{'boxplot'},
##'     \code{'density'}, \code{'histogram'} or \code{'scatterplot'}.
##'     \item fillrange: vector with min and max to apply on fill variable.
##'     Available for \code{out.type = 'heatmap'}, \code{'daily_heatmap'},
##'     \code{'boxplot'}, or \code{'histogram'}.
##'   }
##' 
##' @family plot
##'   
##' @examples
##' library(ncdf4)
##' library(dplyr)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' x.list <- list("model1" = x, "model2" = x)
##' # Simple plot for a single variable and a single model -------------------------
##' 
##' # Latent heat at flux at reference level
##' df <- get_variable(x, "Qle")
##' ggplot_variable(df, out.type = "standard")
##' ggplot_variable(df, out.type = "standard", yrange = c(-100, 1000))
##' ggplot_variable(df, out.type = "daily_heatmap")
##' ggplot_variable(df, out.type = "boxplot")
##' ggplot_variable(df, out.type = "density")
##' ggplot_variable(df, out.type = "histogram")
##' 
##' # Air temperature
##' df <- get_variable(x, "Tair_z")
##' ggplot_variable(df, out.type = "heatmap")
##' ggplot_variable(df, out.type = "standard", color = "nair")
##' df %>%
##'   filter_dim(this.dim = "nair", n.dim.level = 3) %>%
##' ggplot_variable(out.type = "standard", color = "nair")
##' ggplot_variable(df, out.type = "boxplot", x = "nair")
##' 
##' # Root uptake
##' df <- get_variable(x, "root_uptake")
##' ggplot_variable(df, out.type = "heatmap", facet_formula = "~nspecies")
##' df %>%
##'   filter_dim(this.dim = "nsoil", n.dim.level = 4) %>%
##' ggplot_variable(out.type = "standard", color = "nspecies",
##'                 facet_formula = "~nsoil", nrow.facet = 2)
##' 
##' # Plot for two variables and a single model -------------------------
##' 
##' df <- get_two_variables(x, c("Qle","Qh"))
##' ggplot_variable(df, out.type = "scatterplot")
##' ggplot_variable(df, out.type = "scatterplot", bin2d = FALSE)
##' df <- get_two_variables(x, c("Tair_z","wair_z"))
##' ggplot_variable(df, out.type = "scatterplot", bin2d = FALSE, color = "nair")
##' 
##' # Plot for a single variables and two models -------------------------
##' # Only for examples as both models outputs are identical
##' df <- get_variable_comparison(x.list, "Qle")
##' ggplot_variable(df, out.type = "standard", color = "models", linetype = "models")
##' ggplot_variable(df, out.type = "standard", color = "models", facet_formula = "~models")
##' ggplot_variable(df, out.type = "boxplot", fill = "models")
##' ggplot_variable(df, out.type = "histogram", fill = "models")
##' ggplot_variable(df, out.type = "density", color = "models", linetype = "models")
##' 
##' df <- get_variable_comparison(x.list, "w_soil")
##' ggplot_variable(df, out.type = "heatmap", facet_formula = "~models")
##' ggplot_variable(df, out.type = "heatmap", facet_formula = "~models",
##'                 fillrange = c(0,1))
##' 
##' # Plot for two variables and several models -------------------------
##' 
##' df <- get_two_variables(x.list, c("Qle","Qh"))
##' ggplot_variable(df, out.type = "scatterplot", facet_formula = "~models")
##' ggplot_variable(df, out.type = "scatterplot", bin2d = FALSE, 
##'                 color = "models", facet_formula = "~models", shape = "models")
##' df <- get_two_variables(x.list, c("Tair_z","wair_z"), n.air.level = 4)
##' ggplot_variable(df, out.type = "scatterplot", bin2d = FALSE, 
##'                 color = "nair", facet_formula = "~models")
##' 
##' @importFrom ggplot2 ggplot aes geom_line geom_step xlab ylab facet_wrap 
##' scale_color_brewer scale_color_viridis_d theme element_blank theme_dark
##' guide_legend guides element_rect scale_fill_viridis_c scale_y_reverse
##' geom_raster scale_y_continuous scale_x_continuous scale_y_reverse
##' scale_x_datetime geom_hline geom_boxplot geom_bin2d ggtitle element_text
##' coord_cartesian geom_density geom_histogram geom_point
##' @importFrom stringr str_wrap
##' @importFrom scales breaks_pretty breaks_width
##' @importFrom dplyr last mutate ungroup group_by select 
##' @importFrom tidyr pivot_longer
##' @importFrom lubridate hour minute second
##' @importFrom rlang .data
##' @importFrom stats na.omit formula
##' @export
##' 

ggplot_variable <- function(df,
                            time_range,
                            out.type,
                            x, y, color, linetype, shape, fill, facet_formula,
                            xrange = NULL, yrange = NULL, fillrange = NULL,
                            bin2d = TRUE, diffmodels = FALSE, nrow.facet = NULL,
                            format.date = "%b %Y") {
  args <- .check_ggplot_variable(df = df,
                                 time_range = time_range,
                                 out.type = out.type,
                                 x = x, y = y,
                                 color = color, linetype = linetype,
                                 fill = fill, shape = shape, 
                                 facet_formula = facet_formula, 
                                 xrange = xrange,
                                 yrange = yrange,
                                 fillrange = fillrange,
                                 diffmodels = diffmodels, 
                                 nrow.facet = nrow.facet,
                                 bin2d = bin2d,
                                 format.date = format.date)
  for (argi in names(args)) { 
    assign(x = argi, value = args[[argi]]) 
  }
  rm(args)
  this_ylab <- attr_legend(df)
  skip_ylab <- FALSE
  this.title <- NULL
  this_variable <- attr(df, 'var')
  
  
  if (out.type == "standard") {
    
    # standard plot -----------------------------------------------------------
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
                             y = .data[[y]],
                             linetype = .data[[linetype]]))
    } else {
      g <- g + geom_line(aes(x = .data[[x]],
                             y = .data[[y]]))
    }
    
    g <- g +
      scale_x_datetime(NULL, breaks = breaks_pretty(10)) +
      coord_cartesian(ylim = yrange) +
      theme(axis.text.x = element_text(hjust = 1, angle = 30),
            legend.position = "top")
  } else if (out.type == "daily_heatmap") {
    # daily heatmap plot --------------------------------------------------
    skip_ylab <- TRUE
    if (diffmodels) {
      this.legend.fill <- diff.legend
      this.title <- this_ylab
    } else {
      this.legend.fill <- this_ylab
    }
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
                      fill = .data[[fill]])) +
      scale_x_continuous(breaks = breaks_pretty(n = 13), expand = c(0,0)) +
      scale_y_reverse(labels = df_breaks$label,
                      breaks = as.numeric(df_breaks$time)) +
      scale_fill_viridis_c(str_wrap(this.legend.fill, width = 15),
                           option = "D", direction = 1,
                           limits = fillrange) +
      xlab("Time of the day (h)") +
      ylab(NULL)
    
  } else if (out.type == "heatmap") {
    # heatmap plot -----------------------------------------------------------
    skip_ylab <- TRUE
    
    if (diffmodels) {
      this.legend.fill <- diff.legend
      this.title <- this_ylab
    } else {
      this.legend.fill <- this_ylab
    }
    vjust <- 
      switch(y,
             "nsoil" = 1,
             "nveg" = 0,
             "nair" = 0,
             0.5
      )
    g <-
      ggplot(df) +
      geom_raster(aes(x = time,
                      y = .data[[y]],
                      fill = .data[[this_variable]]),
                  vjust = vjust) +
      scale_y_continuous(breaks = breaks_pretty(n = 10)) +
      scale_fill_viridis_c(str_wrap(this.legend.fill, width = 15),
                           option = "D", direction = -1,
                           limits = fillrange) +
      ylab(y) +
      xlab(NULL)
    
    if (y %in% c("nsoil","nair","nveg")) {
      if (y == "nsoil") {
        z_soil <- attr(df, "z_soil")
        dz_soil <- attr(df, "dz_soil")
        bottom_soil <- z_soil$z_soil + 0.5*dz_soil$dz_soil
        if (!is.null(yrange)) {
          keep_levels <- findInterval(x = yrange, vec = 100*c(0,bottom_soil))
          keep_levels[1] <- max(keep_levels[1], 1)
          keep_levels[2] <- min(keep_levels[2], length(z_soil$z_soil))
          z_soil <- z_soil[seq(keep_levels[1], keep_levels[2]),]
          dz_soil <- dz_soil[seq(keep_levels[1], keep_levels[2]),]
          bottom_soil <- z_soil$z_soil + 0.5*dz_soil$dz_soil
          yrange <- keep_levels 
          if (first(keep_levels) == 1) {
            yrange[1] <- 0
          }
        }
        y_levels <-
          breaks_pretty(n = 10)(z_soil$nsoil) %>% 
          as.integer() %>% 
          unique()
        if (first(y_levels) == 0) {
          y_labels <- c(0, bottom_soil[y_levels[-1]])
        } else if (first(y_levels) < first(z_soil$nsoil)) {
          y_levels <- y_levels[-1]
          y_labels <- bottom_soil[which(z_soil$nsoil %in% y_levels)]
        } else {
          y_labels <- bottom_soil[which(z_soil$nsoil %in% y_levels)]
        }
        if (last(y_levels) > last(z_soil$nsoil)) {
          y_levels <- y_levels[-length(y_levels)]
        }
        y_labels <- na.omit(y_labels)
        y_labels <- round(y_labels*100, digits = 1) # in cm
        y_units <- "Depth (cm)"
        
        g <- g +
          scale_y_reverse(y_units,
                          breaks = y_levels,
                          labels = y_labels) 
        if (!is.null(yrange)) {
          g <- g +
            coord_cartesian(ylim = rev(yrange))
        }
        
      } else if (y %in% c("nair","nveg")) {
        veget_top <- attr(df, "veget_height_top") %>% 
          pull(veget_height_top) %>% 
          first()
        zair <- attr(df, "relative_height") %>% 
          mutate(height = relative_height * veget_top)
        dzair <- attr(df, "layer_thickness") 
        top_air <- zair$height + dzair$layer_thickness/2
        
        if (!is.null(yrange)) {
          keep_levels <- findInterval(x = yrange, vec = c(0,top_air))
          keep_levels[1] <- max(keep_levels[1], 1)
          keep_levels[2] <- min(keep_levels[2], length(zair$height))
          yrange <- keep_levels 
          if (first(keep_levels) == 1) {
            yrange[1] <- 0
          }
        }
        y_levels <- breaks_pretty(n = 8)(zair$nair) %>% 
          as.integer() %>% 
          unique()
        y_veg <- which.min(abs(top_air - veget_top))
        if (!(y_veg %in% y_levels)) {
          y_levels <- sort(c(y_levels,y_veg))
        }
        if (first(y_levels) == 0) {
          y_labels <- c(0, top_air[y_levels[-1]])
        } else if (first(y_levels) < first(zair$nair)) {
          y_levels <- y_levels[-1]
          y_labels <- top_air[which(zair$nair %in% y_levels)]
        } else {
          y_labels <- top_air[which(zair$nair %in% y_levels)]
        }
        if (last(y_levels) > last(zair$nair)) {
          y_levels <- y_levels[-length(y_levels)]
        }
        y_labels <- na.omit(y_labels)
        y_labels <- round(y_labels, digits = 1) # in m
        
        y_units <- "Height (m)"
        g <- g +
          geom_hline(yintercept = y_veg, linetype = "dashed")
        g <- g +
          scale_y_continuous(y_units,
                             breaks = y_levels,
                             labels = y_labels)
        g <- g +
          coord_cartesian(ylim = yrange) 
        
        
      }
    }
  } else if (out.type == "boxplot") {
    # boxplot -----------------------------------------------------------
    g <-
      ggplot(df) 
    
    if (is.null(fill) & is.null(x)) {
      g <- g +
        geom_boxplot(aes(x = 1,
                         y = .data[[y]])) 
    } else if (is.null(fill)) {
      g <- g +
        geom_boxplot(aes(x = factor(.data[[x]]),
                         y = .data[[y]]))
    } else if (is.null(x)) {
      g <- g +
        geom_boxplot(aes(x = 1,
                         y = .data[[y]],
                         fill = .data[[fill]]))
    } else {
      g <- g +
        geom_boxplot(aes(x = factor(.data[[x]]),
                         y = .data[[y]],
                         fill = .data[[fill]]))
    }
    if (!is.null(x)) {
      g <- g +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    } else {
      g <- g + 
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
    }
    g <- g +
      coord_cartesian(ylim = yrange) +
      xlab(NULL)
  } else if (out.type == "scatterplot") {
    # scatterplot -----------------------------------------------------------
    skip_ylab <- TRUE
    g <-
      ggplot(df) 
    if (bin2d) {
      g <-  g +
        geom_bin2d(aes(x = .data[[x]],
                       y = .data[[y]])) 
    } else {
      if (is.null(color) & is.null(shape)) {
        g <-  g +
          geom_point(aes(x = .data[[x]],
                         y = .data[[y]]))
      } else if (is.null(color)) {
        g <-  g +
          geom_point(aes(x = .data[[x]],
                         y = .data[[y]],
                         shape = .data[[shape]]))
      } else if (is.null(shape)) {
        g <-  g +
          geom_point(aes(x = .data[[x]],
                         y = .data[[y]],
                         color = .data[[color]])) 
      } else {
        g <-  g +
          geom_point(aes(x = .data[[x]],
                         y = .data[[y]],
                         color = .data[[color]],
                         shape = .data[[shape]]))
      }
    } 
    
    if (length(this_ylab) == 1) {
      this.title <- this_ylab
    } else {
      g <- g +
        scale_x_continuous(this_ylab[1]) +
        scale_y_continuous(this_ylab[2]) 
    }
    
    g <- g +
      coord_cartesian(xlim = xrange,
                      ylim = yrange) 
    
    
  } else if (out.type == "density") {
    
    # density -----------------------------------------------------------------
    
    g <- 
      ggplot(df)
    if (is.null(color) & is.null(linetype)) {
      g <- g +
        geom_density(aes(x = .data[[x]]))
    } else if (is.null(linetype)) {
      g <- g +
        geom_density(aes(x = .data[[x]],
                         color = .data[[color]]))
    } else if (is.null(color)) {
      g <- g +
        geom_density(aes(x = .data[[x]],
                         linetype = .data[[linetype]]))
    } else {
      g <- g +
        geom_density(aes(x = .data[[x]],
                         color = .data[[color]],
                         linetype = .data[[linetype]]))
    }
    g <- g +
      coord_cartesian(xlim = xrange, ylim = yrange) 
  } else if (out.type == "histogram") {
    
    # histogram ---------------------------------------------------------------
    g <- ggplot(df)
    if (is.null(fill)) {
      g <- g +
        geom_histogram(aes(x = .data[[x]]))
    } else {
      g <- g +
        geom_histogram(aes(x = .data[[x]],
                           fill = .data[[fill]]),
                       position = "dodge")
    }
    g <- g +
      coord_cartesian(xlim = xrange, ylim = yrange) 
  }
  
  
  # Return ------------------------------------------------------------------
  if (!is.null(this.title)) {
    g <- g + 
      ggtitle(str_wrap(this.title, width = 100))
  }
  
  if (!is.null(facet_formula)) {
    g <- 
      g +
      facet_wrap(formula(facet_formula), nrow = nrow.facet)
  }
  
  if (!is.null(color) && color == "nspecies") {
    g <- g +
      scale_color_brewer("Species", palette = "Set1") 
  }
  
  if ( !skip_ylab ) {
    if (!diffmodels) {
      g <- g + ylab(str_wrap(this_ylab, width = 25))
    } else {
      g <- g + 
        ggtitle(str_wrap(this_ylab, width = 75)) +
        ylab(str_wrap(diff.legend, width = 25))
    }
  }
  
  g
}

.check_ggplot_variable <-
  function(df, time_range, out.type,
           x, y,
           color, linetype, fill, shape, facet_formula,
           xrange, yrange, fillrange,
           diffmodels, nrow.facet,
           bin2d,
           format.date) {

    if (missing(out.type)) {
      out.type <- "standard"
    }
    .fun_testIfIn(out.type, values =  c("standard",
                                        "heatmap",
                                        "daily_heatmap",
                                        "boxplot",
                                        "density",
                                        "histogram",
                                        "scatterplot"))
    # variable & out.type check ------------------------------------------
    this_variable <- attr(df, 'var')
    if (length(this_variable) > 1 &
        out.type != "scatterplot") {
      stop("Plotting multiple variable at once is only possible with `out.type = 'scatterplot'`")
    }  
    
    # input data.frame --------------------------------------------------------
    
    diff.legend <- NULL
    if (diffmodels) {
      listmodels <- attr(df, "models")[1:2]
      attr(df, "models") <- NULL
      diff.legend <-  paste0(
        "Difference between ", listmodels[1],
        " and ", listmodels[2])
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
    
    
    # Potential dim -----------------------------------------------------------
    potential_dim <- attr(df, "dimname")
    full_dim <- c(potential_dim, this_variable)
    listmodels <- attr(df, "models")
    if (out.type %in% c("boxplot", "density", "histogram", "scatterplot")) {
      potential_dim <- str_subset(potential_dim, "time", negate = TRUE)
    }
    
    if (missing(x)) x <- NULL
    if (missing(y)) y <- NULL
    if (missing(color)) color <- NULL
    if (missing(linetype)) linetype <- NULL
    if (missing(fill)) fill <- NULL
    if (missing(shape)) shape <- NULL
    allargs <- c(x, y, color, linetype, fill, shape)
    if (any(!is.null(allargs))) {
      potential_dim <-
        str_subset(potential_dim,
                   paste0(allargs, collapse = "|"),
                   negate = TRUE)
    }
    # x -----------------------------------------------------------------------
    
    if (out.type %in% c("standard", "heatmap")) {
      # x arg ignored
      x <- "time"
      potential_dim <- str_subset(potential_dim, "time", negate = TRUE)
    } else if (out.type %in% c("scatterplot")) {
      # x and y managed together
      # x and y arg ignored
      if (length(listmodels) == 2 & length(this_variable) == 1) {
        x <- listmodels[1]
        y <- listmodels[2]
        df.save <- df
        df <- 
          df %>% 
          pivot_wider(names_from = "models",
                      values_from = sym(this_variable))
        
        attr(df, "var") <- attr(df.save, "var")
        attr(df, "units") <- attr(df.save, "units")
        attr(df, "longname") <- attr(df.save, "longname")
        attr(df, "dimname") <- attr(df.save, "dimname")
        attr(df, "ndim") <- length(attr(df, "dimname"))
        attr(df, "nvar") <- ncol(df) - attr(df, "ndim")
        attr(df, "models") <- names(x) 
        attr(df, "z_soil") <- attr(df.save, "z_soil")
        attr(df, "dz_soil") <- attr(df.save, "dz_soil")
        attr(df, "relative_height") <- attr(df.save, "relative_height")
        attr(df, "veget_height_top") <- attr(df.save, "veget_height_top")
        attr(df, "layer_thickness") <- attr(df.save, "layer_thickness")
        rm(df.save)
        
        potential_dim <- str_subset(potential_dim, "models", negate = TRUE)
      } else if (length(this_variable) == 2) {
        x <- this_variable[1]
        y <- this_variable[2]
      } else {
        stop("Scatterplot can only be used when comparing two models with one variable or two variables with any models")
      }
    } else if (out.type %in% c("boxplot")) {
      if (missing(x) || is.null(x) || x == "") {
        if (length(potential_dim) > 0) {
          x <- first(potential_dim)
          potential_dim <- str_subset(potential_dim, x, negate = TRUE)
        } else {
          x <- NULL
        }
      } else {
        .fun_testIfIn(x, full_dim)
        potential_dim <- str_subset(potential_dim, x, negate = TRUE)
      }
    } else if (out.type %in% c("density", "histogram")) {
      stopifnot(length(this_variable) == 1)
      x <- this_variable
    } else if (out.type %in% c("daily_heatmap")) {
      # manage both x and y
      # x and y argument ignored
      potential_dim <- str_subset(potential_dim, "time", negate = TRUE)
      x <- NULL
      y <- NULL
    }
    
    
    # y -----------------------------------------------------------------------
    
    if (out.type %in% c("standard", "boxplot")) {
      # y arg ignored
      stopifnot(length(this_variable) == 1)
      y <- this_variable
    } else if (out.type %in% c("density", 'histogram')) {
      # y argument ignored
      y <- NULL
    } else if (out.type %in% c("heatmap")) {
      if (missing(y) || is.null(y) || y == "") {
        subdim <- str_subset(potential_dim, "nsoil|nair|nveg")
        if (length(subdim) > 0) {
          y <- first(subdim)
        } else {
          stop("heatmap can only be used when variable have nsoil, nair or nveg dimension")
        }
      } else {
        .fun_testIfIn(y, c("nsoil","nair","nveg"))
      }
      potential_dim <- str_subset(potential_dim, y, negate = TRUE)
    } 
    
    
    # fill -----------------------------------------------------------------
    
    if (out.type %in% c("standard", "scatterplot", "density")) {
      # fill arg ignored
      fill <- NULL
    } else if (out.type %in% c("boxplot", 'histogram')) {
      if (missing(fill) || is.null(fill) || fill == "") {
        if (length(potential_dim) > 0) {
          fill <- NULL
          # fill <- first(potential_dim)
          # potential_dim <- str_subset(potential_dim, fill, negate = TRUE)
        } else {
          fill <- NULL
        }
      } else {
        .fun_testIfIn(fill, full_dim)
        potential_dim <- str_subset(potential_dim, fill, negate = TRUE)
      }
    } else if (out.type %in% c("heatmap", "daily_heatmap")) {
      # fill arg ignored
      fill <- this_variable
    }
    
    
    # color -------------------------------------------------------------------
    
    if (out.type %in% c("standard", "density") ||
        (out.type == "scatterplot" & !bin2d)) {
      if (missing(color) || is.null(color) || color == "") {
        if (length(potential_dim) > 0) {
          color <- NULL
          # color <- first(potential_dim)
          # potential_dim <- str_subset(potential_dim, color, negate = TRUE)
        } else {
          color <- NULL
        }
      } else {
        .fun_testIfIn(color, full_dim)
        potential_dim <- str_subset(potential_dim, color, negate = TRUE)
      }
    } else {
      # color arg ignored
      color <- NULL
    } 
    
    # linetype ----------------------------------------------------------------
    
    if (out.type %in% c("standard", "density")) {
      if (missing(linetype) || is.null(linetype) || linetype == "") {
        if (length(potential_dim) > 0) {
          linetype <- NULL
          # linetype <- first(potential_dim)
          # potential_dim <- str_subset(potential_dim, linetype, negate = TRUE)
        } else {
          linetype <- NULL
        }
      } else {
        .fun_testIfIn(linetype, full_dim)
        potential_dim <- str_subset(potential_dim, linetype, negate = TRUE)
      }
    } else if (out.type %in% c("boxplot", "histogram", "scatterplot",
                               "heatmap", "daily_heatmap")) {
      # linetype arg ignored
      linetype <- NULL
    } 
    
    
    # shape ----------------------------------------------------------------
    
    if (out.type %in% c("scatterplot") & !bin2d) {
      if (missing(shape) || is.null(shape) || shape == "") {
        if (length(potential_dim) > 0) {
          shape <- NULL # no default shape
          # shape <- first(potential_dim)
          # potential_dim <- str_subset(potential_dim, shape, negate = TRUE)
        } else {
          shape <- NULL
        }
      } else {
        .fun_testIfIn(shape, full_dim)
        potential_dim <- str_subset(potential_dim, shape, negate = TRUE)
      }
      
    } else {
      # shape arg ignored
      shape <- NULL
    } 
    
    # facet ----------------------------------------------------------------
    if (missing(facet_formula) || is.null(facet_formula)) {
      if (length(potential_dim) > 0) {
        facet_formula <- NULL
        # facet_formula <- paste0("~", paste0(potential_dim, collapse = "+"))
        # potential_dim <- NULL
      } else {
        facet_formula <- NULL
      }
    } else if (first(facet_formula == "")) {
      facet_formula <- NULL
    } else if (!grepl("~", first(facet_formula), fixed = TRUE)) {
      facet_formula <- paste0("~", paste0(facet_formula, collapse = "+"))
      potential_dim <- str_subset(potential_dim, 
                                  paste0(facet_formula, collapse = "|"),
                                  negate = TRUE)
    }
    # Remaining dimension
    
    # if (out.type %in% c("standard","heatmap", "daily_heatmap","histogram") &
    #     length(potential_dim) > 0) {
    #   stop("Remaining dimension needs to be handled appropriately: ", 
    #        paste0(potential_dim, collapse = " ; "))
    # }  
    
    if (any(is.na(xrange))) {
      xrange <- NULL
    }
    if (any(is.na(yrange))) {
      yrange <- NULL
    }
    if (any(is.na(fillrange))) {
      fillrange <- NULL
    }
    
    
    if (!is.null(color) && 
        length(unique(df[,color])) <= 15) {
      df[,color] <- factor(df[,color, drop = TRUE])
    }
    
    
    if (!is.null(shape)) {
      df[,shape] <- factor(df[,shape, drop = TRUE])
    }
    if (!is.null(linetype)) {
      df[,linetype] <- factor(df[,linetype, drop = TRUE])
    }
    if (!is.null(fill) &&
        out.type %in% c("boxplot",
                        "histogram")) {
      df[,fill] <- factor(df[,fill, drop = TRUE])
    }
    
    if (is.null(nrow.facet)) {
      if (out.type %in% c("heatmap", "daily_heatmap")) {
        nrow.facet <- 1
      }
    }
    
    list(df = df,
         format.date = format.date,
         out.type = out.type,
         x = x,
         y = y,
         color = color,
         linetype = linetype,
         shape = shape,
         fill = fill,
         facet_formula = facet_formula,
         xrange = xrange,
         yrange = yrange,
         fillrange = fillrange,
         diff.legend = diff.legend)
  }

# ggplot_list_var ---------------------------------------------------------
##' @name ggplot_list_var
##' @author Remi Lemaire-Patin
##' 
##' @title plot for a list of variable
##' 
##' @description \code{ggplot_list_var} is a wrapper around \code{\link{ggplot_variable}} to produce plots for a list of variables. Plots are aggregated using \code{plot_grid}
##' 
##' @inheritParams get_variable
##' @param x a \code{ncdf4} object. Output generated by MuSICA
##' @param list_var a \code{character vector} of variable names to be plotted
##' 
##' @return
##' 
##' A \code{grid} object with all plots aggregated.
##' 
##' @family plot
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' ggplot_list_var(x, list_var = c("Qle","Qh","Qg"))
##' 
##' @importFrom cowplot plot_grid
##' @export
##' 
##' 

ggplot_list_var <- function(x, list_var, time_range) {
  list_plot <- list()
  for (this_var in list_var) {
    df.try <- try({ 
      df <- get_variable(x, this_var, time_range = time_range)
      this_ggplot <-
        ggplot_variable(df)
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
##' @inheritParams get_variable 
##' @inheritParams ggplot_variable
##' @param pixheight height of the \code{dygraph} object in pixel
##' @param pixwidth width of the \code{dygraph} object in pixel
##' @param axisLabelWidth height of the y axis label
##' @param group \code{character}, name of the group to synchronize multiple
##'   dygraphs
##' @return
##' 
##' A \code{dygraph} object
##' 
##' @family plot
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' df <- get_variable(x, "Qle")
##' dygraph_variable(df,  pixwidth = 600)
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
##' @inheritParams ggplot_variable
##' @inheritParams dygraph_variable
##' @param main.title (\emph{optional}) a \code{character} with plot title prefix
##' @return
##' 
##' A \code{dygraph} object
##' 
##' @family plot
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' x.list <- list("model1" = x, "model2" = x)
##' df <- get_variable_comparison(x.list, varname = "Qle")
##' dygraph_comparison(df,  pixwidth = 600)
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
                               main.title = NULL,
                               diffmodels = FALSE) {
  
  if (!missing(time_range)) {
    df <-
      df %>% 
      filter(time >= time_range[1],
             time <= time_range[2])
  }
  
  this_variable <- attr(df, "var")
  keep_attr <- attributes(df)
  if (!is.null(attr(df, "models")) & !diffmodels) {
    df <- pivot_wider(df, 
                      values_from = this_variable,
                      names_from = "models")  
    for (this.attr in c("units", "longname", "var",
                        "ndim", "dimname", "nvar", "models")) {
      attr(df, this.attr) <- keep_attr[[this.attr]]
    }
    attr(df, "ndim") <- attr(df, "ndim") - 1
  }
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
      this.xts <- xts(select(this.df, -time),
                      order.by = this.df$time)
    } else {
      if (is.null(main.title)) {
        this.main <- paste0(first(colnames(this.df)), " = ", first(this.df[,1]))
      }
      this.df <- this.df[,-1]
      this.xts <- xts(this.df[, -which(colnames(this.df) == "time")],
                      order.by = this.df$time)
    }
    if (diffmodels) {
      listmodels <- attr(df, "models")[1:2]
      attr(df, "models") <- NULL
      if (!is.null(main.title)) {
        this.main <-  paste0(this.main, " ; ")
      }
      this.main <-  paste0(this.main,
                           "Difference between ", listmodels[1],
                           " and ", listmodels[2])
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
