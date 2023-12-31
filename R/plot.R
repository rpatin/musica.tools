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
##' scale_x_datetime geom_hline geom_boxplot geom_bin2d ggtitle element_text
##' coord_cartesian geom_density geom_histogram geom_point
##' @importFrom stringr str_wrap
##' @importFrom scales breaks_pretty breaks_width
##' @importFrom dplyr last mutate ungroup group_by select 
##' @importFrom tidyr pivot_longer
##' @importFrom lubridate hour minute second
##' @export
##' 

ggplot_variable <- function(df,
                            time_range,
                            format.date = "%b %Y",
                            out.type,
                            x, y, color, linetype, shape, fill, facet_formula,
                            xrange = NULL, yrange = NULL, fillrange = NULL,
                            bin2d = TRUE, diffmodels = FALSE) {
  # df <- list_value$relative_height
  args <- .check_ggplot_variable(df = df,
                                 time_range = time_range,
                                 format.date = format.date,
                                 out.type = out.type,
                                 bin2d = bin2d,
                                 x = x, y = y,
                                 color = color, linetype = linetype,
                                 fill = fill, shape = shape, 
                                 facet_formula = facet_formula, 
                                 xrange = xrange,
                                 yrange = yrange,
                                 fillrange = fillrange,
                                 diffmodels = diffmodels)
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
                             color = factor(.data[[color]]),
                             linetype = factor(.data[[linetype]])))
    } else if (!is.null(color)) {
      g <- g + geom_line(aes(x = .data[[x]],
                             y = .data[[y]],
                             color = factor(.data[[color]])))
      
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
        if (!is.null(yrange)) {
          g <- g +
            coord_cartesian(ylim = yrange) 
        }
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
        geom_boxplot(aes(x = .data[[x]],
                         y = .data[[y]]))
    } else if (is.null(x)) {
      g <- g +
        geom_boxplot(aes(x = 1,
                         y = .data[[y]],
                         fill = .data[[fill]]))
    } else {
      g <- g +
        geom_boxplot(aes(x = .data[[x]],
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
      } else if (!is.null(shape)) {
        g <-  g +
          geom_point(aes(x = .data[[x]],
                         y = .data[[y]],
                         shape = .data[[shape]]))
      } else if (!is.null(color)) {
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
    
    if (!is.null(xrange)) {
      g <- g +
        coord_cartesian(xlim = xrange) 
    }
    if (!is.null(yrange)) {
      g <- g +
        coord_cartesian(ylim = yrange)
    }
    
  } else if (out.type == "density") {
    
    # density -----------------------------------------------------------------
    
    g <- 
      ggplot(df)
    if (is.null(color) & is.null(linetype)) {
      g <- g +
        geom_density(aes(x = .data[[x]]))
    } else if (!is.null(color)) {
      g <- g +
        geom_density(aes(x = .data[[x]],
                         color = .data[[color]]))
    } else if (!is.null(linetype)) {
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
      coord_cartesian(xlim = xrange) +
      coord_cartesian(ylim = yrange)
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
      coord_cartesian(xlim = xrange) +
      coord_cartesian(ylim = yrange)
  }
  
  
  # Return ------------------------------------------------------------------
  if (!is.null(this.title)) {
    g <- g + 
      ggtitle(str_wrap(this.title, width = 100))
  }
  
  if (!is.null(facet_formula)) {
    g <- 
      g +
      facet_wrap(formula(facet_formula), nrow = 1)
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
  function(df, time_range, format.date, out.type, bin2d,
           x, y,
           color, linetype, fill, shape, facet_formula,
           xrange, yrange, fillrange,
           diffmodels) {
    
    if (missing(out.type)) {
      out.type <- "standard"
    }
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
      } else if (length(this_variable) == 2 & length(listmodels) == 1) {
        x <- this_variable[1]
        y <- this_variable[2]
      } else {
        stop("Scatterplot can only be used when comparing two models or two variables")
      }
    } else if (out.type %in% c("boxplot")) {
      if (missing(x) || is.null(x)) {
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
      if (missing(y) || is.null(y)) {
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
      if (missing(fill) || is.null(fill)) {
        if (length(potential_dim) > 0) {
          fill <- first(potential_dim)
          potential_dim <- str_subset(potential_dim, fill, negate = TRUE)
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
      if (missing(color) || is.null(color)) {
        if (length(potential_dim) > 0) {
          color <- first(potential_dim)
          potential_dim <- str_subset(potential_dim, color, negate = TRUE)
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
      if (missing(linetype) || is.null(linetype)) {
        if (length(potential_dim) > 0) {
          linetype <- first(potential_dim)
          potential_dim <- str_subset(potential_dim, linetype, negate = TRUE)
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
      if (missing(shape) || is.null(shape)) {
        if (length(potential_dim) > 0) {
          shape <- first(potential_dim)
          potential_dim <- str_subset(potential_dim, shape, negate = TRUE)
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
        facet_formula <- paste0("~", paste0(potential_dim, collapse = "+"))
        potential_dim <- NULL
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
    
    if (out.type %in% c("standard","heatmap", "daily_heatmap","histogram") &
        length(potential_dim) > 0) {
      stop("Remaining dimension needs to be handled appropriately: ", 
           paste0(potential_dim, collapse = " ; "))
    }  
    
    if (any(is.na(xrange))) {
      xrange <- NULL
    }
    if (any(is.na(yrange))) {
      yrange <- NULL
    }
    if (any(is.na(fillrange))) {
      fillrange <- NULL
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
                               main.title = NULL,
                               diffmodels = FALSE) {
  
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
        this.main <- paste0(first(colnames(this.df)), " = ", first(this.df[,1]))
      }
      this.df <- this.df[,-1]
      this.xts <- xts(this.df[, -which(colnames(this.df) == "time")],
                      order.by = this.df$time)
    }
    if (diffmodels) {
      listmodels <- attr(df, "models")[1:2]
      attr(df, "models") <- NULL
      this.main <-  paste0(this.main, " ; ",
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
