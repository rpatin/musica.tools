# get_timeline ---------------------------------------------------------
##' @name get_timeline
##' @author Remi Lemaire-Patin
##' 
##' @title Get a timeline from a \code{ncdf4} object
##' 
##' @description This function produce a timeline for available data based on a
##'   \code{ncdf4} object and a list of variables
##' 
##' 
##' @param x a \code{ncdf4} object. Input data
##' @param varnames a \code{character}, the variable to extract
##' @param time_range (\emph{optional}) a \code{POSIXct vector} with two values:
##'   the time range to extract
##' @return
##' 
##' A \code{data.frame} 
##' 
##' @family getters
##'
##' @examples
##' library(ncdf4)
##' @export


get_timeline <- function(x, varnames, time_range = NULL) {
  
  this.obs <- foreach(this.var = varnames, .combine = 'inner_join') %do% {
    get_variable(x, this.var, time_range = time_range)
  }
  
  long.obs <- 
    this.obs %>% 
    pivot_longer(cols = everything() & !time,
                 names_to = "variable",
                 values_to = "values") %>% 
    mutate(has.values = !is.na(values) & !is.nan(values),
           has.values = as.numeric(has.values)) %>% 
    arrange(variable, time)
  df.timeline <- get_timeline_df(long.obs)
  df.timeline
}


# plot_timeline ---------------------------------------------------------
##' @name plot_timeline
##' @author Remi Lemaire-Patin
##' 
##' @title plot a timeline 
##' 
##' @description This function plot a timeline based on the output of
##'   \code{\link{get_timeline}}
##' 
##' 
##' @param df a \code{data.frame} object,  output of \code{\link{get_timeline}}
##' @return
##' 
##' A \code{ggplot} object 
##' 
##' @family plot
##'
##' @examples
##' library(ncdf4)
##' @export



plot_timeline <- function(df,
                          linewidth = 2,
                          nbreaks = 25) {
  df %>% 
    mutate(variable = factor(variable,
                             levels = rev(unique(variable)))) %>% 
    ggplot() + 
    geom_linerange(aes(xmin = time_begin, xmax = time_end, y = variable), 
                   linewidth = linewidth) +
    scale_color_discrete(NULL) +
    scale_x_datetime(breaks = breaks_pretty(n = nbreaks)) +
    theme_bw() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    ylab(NULL)
  
}

# get_timeline_df ---------------------------------------------------------
##' @name get_timeline_df
##' @author Remi Lemaire-Patin
##' 
##' @title get a timeline 
##' 
##' @description This function produce a timeline data.frame to feed 
##' \code{\link{plot_timeline}}
##' 
##' 
##' @param df a \code{data.frame} object,  with columns \code{time}
##'   (\code{POSIXct}), \code{variable} (\code{character)}) and
##'   \code{has.values} (\code{1} or \code{0})
##' @return
##' 
##' A \code{data.frame} object 
##' 
##' @family getters
##'
##' @examples
##' library(ncdf4)
##' @export

get_timeline_df <- function(df) {
  split.obs <- split(df, df$variable)
  df.timeline <- lapply(split.obs, function(this.df){
    this.rle <- rle(this.df$has.values)
    this.end <- cumsum(this.rle$lengths)
    this.begin <- c(1, this.end[1:(length(this.end) - 1)])
    this.end <- this.end[which(this.rle$values == 1)]
    this.begin <- this.begin[which(this.rle$values == 1)]
    data.frame(variable = first(this.df$variable),
               time_begin = this.df$time[this.begin],
               time_end = this.df$time[this.end])
  }) %>% 
    do.call('rbind', .)
  df.timeline
}
