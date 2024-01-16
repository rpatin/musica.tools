# get_dim_info ---------------------------------------------------------
##' @name get_dim_info
##' @author Remi Lemaire-Patin
##' 
##' @title Get information on ncdf dimensions
##' 
##' @description This function extract dimension id from the \code{ncdf4} object
##' 
##' 
##' @param x a \code{ncdf4} object
##' 
##' @return
##' 
##' A \code{data.frame}
##' 
##' 
##' @family Tools
##' 
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' get_dim_info(x)
##' @export
##' 
##' 


get_dim_info <- function(x) {
  dimlist <- (lapply(x$dim, function(x) x$id))
  data.frame(dimname = names(dimlist),
             id = unlist(dimlist),
             row.names = NULL)
}

# get_dim_value ---------------------------------------------------------
##' @name get_dim_value
##' @author Remi Lemaire-Patin
##' 
##' @title Get dimension value from \code{ncdf4} object
##' 
##' @description This function extract dimension value from the \code{ncdf4} object
##' 
##' 
##' @param x a \code{ncdf4} object
##' @param dimname a \code{character} name of a netCDF dimension
##' 
##' @return
##' 
##' A \code{vector}
##' 
##' @family Tools
##' 
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' get_dim_value(x, "time")
##' get_dim_value(x, "nsoil")
##' @export
##' @importFrom ncdf4 ncatt_get
##' 


get_dim_value <- function(x, dimname) {
  .check_dim_value(
    x = x,
    dimname = dimname
  )
  return_values <- x$dim[[dimname]]$vals
  
  if (dimname %in% c("time")) {
    return_values <- format_time(return_values, date0 = x$dim[[dimname]]$units)
  }
  
  if (dimname %in% c("n_time")) { #then it is dev_jerome
    return_values <- ncvar_get(x, "time")
    return_values <- format_time(return_values, date0 = x$var[["time"]]$units)
  }
  
  if (dimname %in% c("nspecies", "n_species_max")) {
    list_species <- ncatt_get(x, varid = 0)
    return_values <- factor(return_values, 
                            levels = return_values, 
                            labels = unlist(list_species))
  }
  return_values
}

.check_dim_value <- function(x, dimname) {
  if (!inherits(x,"ncdf4")) {
    stop("x must be a ncdf4 object")
  }
  if ( length(dimname) != 1) {
    stop("dimname must be of length 1")
  }
  dfdim <- get_dim_info(x)
  if (!dimname %in% dfdim$dimname) {
    stop("dimension ", dimname, " not found in x")
  }
  NULL
}

# format_time ---------------------------------------------------------
##' @name format_time
##' @author Remi Lemaire-Patin
##' 
##' @title format netcdf time into POSIXct
##' 
##' @description Reformat the netcdf time "hours since 2009-01-01 00:00:00+00:00"
##' into a proper POSIXct class
##' 
##' 
##' @param time a \code{numeric} in hours since Date0 format$
##' @param date0 a \code{string}, reference date
##' 
##' @return
##' 
##' A \code{POSIXct} vector
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' format_time(x$dim[["time"]]$vals, date0 = x$dim[["time"]]$units)
##' @importFrom lubridate ymd_hms
##' @importFrom stringi stri_split_fixed
##' @export


format_time <- function(time, date0) {
  if (grepl("(GMT", date0, fixed = TRUE)) { # for dev_jerome
    date0 <- stri_split_fixed(str = date0, pattern = " (GMT+")[[1]][1]
    date0  <- ymd_hms(date0) - 1800
  } else {
    date0  <- ymd_hms(date0)
  }
  date0 + time*3600. # add seconds after date0
}


# attr_legend ---------------------------------------------------------
##' @name attr_legend
##' @author Remi Lemaire-Patin
##' 
##' @title Extract legend from \code{data.frame} attributes
##' 
##' @description Extract a proper formatted legend from the attributes stored
##' in a \code{data.frame} 
##' 
##' @param df a \code{data.frame}, output of \code{\link{get_variable}}
##' 
##' @return
##' 
##' A \code{POSIXct} vector
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' df <- get_variable(x, "Qle")
##' attr_legend(df)
##' @importFrom lubridate ymd_hms
##' @export


attr_legend <- function(df) {
  paste0(attr(df, "longname"), " (", attr(df,"units"),")")
}

# .fun_testIfIn ---------------------------------------------------------
##' @name .fun_testIfIn
##' 
##' @title Check that value is within a range of option
##' 
##' @description Generic function to check wether a value is within a range of
##' option or not. Output appropriate error message if the check failed. Original
##' code from \code{biomod2} package
##' 
##' @param x a scalar value, \code{character} or \code{numeric}
##' @param values a \code{vector} (\code{character} or \code{numeric}), list
##' of accepted values for \code{x}
##' @return
##' 
##' A \code{boolean}
##' 
##' @family Tools
##'   
##' @examples
##' #  library(ncdf4)
##' #  x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' #  .fun_testIfIn("Qle", names(x$var))
##'
##'


.fun_testIfIn <- function(x, values)
{
  x.name <- deparse(substitute(x))
  if (any(!(x %in% values))) {
    stop(paste0("\n", x.name, " must be '", 
                ifelse(length(values) > 1, 
                       paste0(paste0(values[1:(length(values) - 1)], collapse = "', '"),
                              "' or '", values[length(values)])
                       , paste0(values,"'"))))
  }
  TRUE
}


# filter_dim ---------------------------------------------------------
##' @name filter_dim
##' @author Remi Lemaire-Patin
##' 
##' @title Filter a \code{data.frame} dimension
##' 
##' @description Filter a data frame along dimension \code{this.dim}, either by
##'   providing an aimed number of levels (through argument \code{n.dim.level})
##'   or by providing the levels to keep (through argument \code{list.dim}). If
##'   both \code{n.dim.level} and \code{list.dim} are NULL, then no filtering
##'   occurs.
##' 
##' @param df a \code{data.frame}
##' @param this.dim a \code{character}, the dimension to filter
##' @param n.dim.level a \code{numeric}, the max. number of level to keep.
##' @param list.dim a \code{vector}, the specific levels to keep. 
##' 
##' @return
##' 
##' A \code{data.frame}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' df <- get_variable(x, "root_uptake")
##' filter_dim(df, this.dim = "nsoil", n.dim.level = 4)
##' filter_dim(df, this.dim = "nsoil", list.dim = c(1, 5, 10))
##' filter_dim(df, this.dim = "nspecies", list.dim = c("pinus_pinaster"))
##' @importFrom rlang sym `!!`
##' @export


filter_dim <- function(df, this.dim, 
                       n.dim.level = NULL, list.dim = NULL) {
  if (any(colnames(df) == this.dim) & (!is.null(n.dim.level) | !is.null(list.dim))) {
    doFilter <- FALSE
    if (is.null(list.dim)) {
      list.dim <- unique(df[, this.dim])
      n.list.dim <- length(list.dim)
      if (n.list.dim > n.dim.level) {
        doFilter <- TRUE
        list.dim <- c(list.dim[floor(seq(1, n.list.dim, length.out = n.dim.level))])
      }
    } else {
      list.dim <- list.dim[list.dim %in% unique(df[, this.dim])]
      doFilter <- TRUE
    }
    if (doFilter) {
      df <- 
        df %>% 
        filter(!!sym(this.dim) %in% list.dim)
      if (length(list.dim) == 1) {
        df <- 
          df %>% 
          select(-sym(this.dim))
        list.dimname <- attr(df, "dimname")
        attr(df, "dimname") <- list.dimname[-which(list.dimname == this.dim)]
      }
    }
  }
  df
}



# var_with_dim ---------------------------------------------------------
##' @name var_with_dim
##' @author Remi Lemaire-Patin
##' 
##' @title Return variable names with a given dimension
##' 
##' @description Extract from a MuSICA \code{netcdf} output the list of
##'   variables that share a given dimension.
##' 
##' @param x a \code{ncdf4} object. Output generated by MuSICA
##' @param this.dim a \code{character}, the dimension of interest
##' 
##' @return
##' 
##' A \code{vector}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' var_with_dim(x, "time")
##' var_with_dim(x, "nsoil")
##' @export

var_with_dim <- function(x, this.dim) {
  out <- sapply(names(x$var), function(this.var) {
    this.dim %in% get_variable(x, this.var, return.colnames = TRUE)
  })
  if (any(out)) {
    return(names(out)[which(out)])
  } else {
    return(NULL)
  }
}


# list_dim_allvar ---------------------------------------------------------
##' @name list_dim_allvar
##' @author Remi Lemaire-Patin
##' 
##' @title List all variables dimensions
##' 
##' @description List dimensions for all variables contained in a MuSICA
##'   \code{netcdf} output.
##' 
##' @param x a \code{ncdf4} object. Output generated by MuSICA
##' 
##' @return
##' 
##' A \code{list} of \code{vector}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' list_dim_allvar(x)
##' @export

list_dim_allvar <- function(x) {
  sapply(names(x$var), function(this.var) {
    get_variable(x, this.var, return.colnames = TRUE)
  })
}

# var_with_same_dim ---------------------------------------------------------
##' @name var_with_same_dim
##' @author Remi Lemaire-Patin
##' 
##' @title List variables with same dimensions
##' 
##' @description List all variables with the exact same dimensions as a given
##'   variable.
##' 
##' @param x a \code{ncdf4} object. Output generated by MuSICA
##' @param this.var a \code{character}, the variables for which other variables
##'   with the same dimensions will be returned
##' 
##' @return
##' 
##' A character \code{vector}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' var_with_same_dim(x, this.var = "Qle")
##' var_with_same_dim(x, this.var = "transpir")
##' @export
##' @importFrom foreach foreach `%do%`

var_with_same_dim <- function(x, this.var) {
  if (inherits(x, "list")) {
    x <- x[[1]]
  }
  list.dim <- list_dim_allvar(x)
  aimed.dim <- list.dim[[this.var]]
  tmp <- sapply(list.dim, function(this.dim){
    length(aimed.dim) == length(this.dim) &&
      all(aimed.dim %in% this.dim)
  })
  list.names <- names(tmp)[tmp]
  str_subset(list.names, this.var, negate = TRUE)
}

# get_time_range ---------------------------------------------------------
##' @name get_time_range
##' @author Remi Lemaire-Patin
##' 
##' @title get time range of a \code{ncdf4} or list of \code{ncdf4} object
##' 
##' @description This function extract the time range of a \code{ncdf4} or a list of 
##' \code{ncdf4} objects
##' 
##' 
##' @param musica.list a \code{ncdf4} or  a named list \code{ncdf4} object.
##' 
##' @return
##' 
##' A \code{POSIXct vector}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' x.list <- list("model1" = x, "model2" = x)
##' get_time_range(x)
##' get_time_range(x.list)
##' 
##' @export
##' 
##' 

get_time_range <- function(musica.list) {
  if (inherits(musica.list, 'ncdf4')) {
    return( 
      range(get_dim_value(musica.list, "time"))
    )
  } else {
    return(
      range(get_dim_value(musica.list[[1]], "time"))
    )
  }
}


# get_6month ---------------------------------------------------------
##' @name get_6month
##' @author Remi Lemaire-Patin
##' 
##' @title Get 6 month time range
##' 
##' @description This function extract the first 6 month time range of a 
##' \code{ncdf4} or list of \code{ncdf4} objects
##' 
##' @inheritParams get_time_range
##' 
##' @return
##' 
##' A \code{POSIXct vector}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' x.list <- list("model1" = x, "model2" = x)
##' get_6month(x)
##' get_6month(x.list)
##' 
##' @export
##' 
##' 

get_6month <- function(musica.list) {
  this.range <- get_time_range(musica.list)
  this.min <- this.range[1]
  this.max <- min(this.range[2], this.range[1] + 24*3600*180)
  c(this.min, this.max)
}

# get_soil_level ---------------------------------------------------------
##' @name get_soil_level
##' @author Remi Lemaire-Patin
##' 
##' @title get soil layer bottom and top depth
##' 
##' @description This function return bottom and top depth of a single soil
##'   layer
##' 
##' 
##' @param x a \code{ncdf4} object. Output generated by MuSICA
##' @param soil.level a \code{numeric}, soil level to keep.
##' 
##' @return
##' 
##' A \code{ggplot} object
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' get_soil_level(x, soil.level = 1)
##' get_soil_level(x, soil.level = 10)
##' get_soil_level(x, soil.level = 60)
##' @importFrom dplyr pull
##' @export
##' 
##' 


get_soil_level <- function(x, soil.level) {
  stopifnot(length(soil.level) == 1)
  zsoil <- get_variable(x, "z_soil") %>% 
    filter(nsoil == soil.level) %>% 
    pull(z_soil)
  dzsoil <- get_variable(x, "dz_soil") %>% 
    filter(nsoil == soil.level) %>% 
    pull(dz_soil)
  zsoil + c(-dzsoil/2, dzsoil/2)
}

# get_air_level ---------------------------------------------------------
##' @name get_air_level
##' @author Remi Lemaire-Patin
##' 
##' @title get air layer bottom and top height
##' 
##' @description This function return bottom and top height of a single air
##'   layer
##' 
##' 
##' @param x a \code{ncdf4} object. Output generated by MuSICA
##' @param air.level a \code{numeric}, air level to keep.
##' 
##' @return
##' 
##' A \code{ggplot} object
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
##' get_air_level(x, air.level = 1)
##' get_air_level(x, air.level = 10)
##' get_air_level(x, air.level = 15)
##' @importFrom dplyr pull
##' @export
##' 
##' 


get_air_level <- function(x, air.level) {
  stopifnot(length(air.level) == 1)
  veget_top <- get_variable(x, "veget_height_top") %>% 
    pull(veget_height_top) %>% 
    first()
  zair <- get_variable(x, "relative_height") %>% 
    filter(nair == air.level) %>% 
    pull(relative_height) * veget_top
  dzair <- get_variable(x, "layer_thickness") %>% 
    filter(nair == air.level) %>% 
    pull(layer_thickness)
  zair + c(-dzair/2, dzair/2)
}
