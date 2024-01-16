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
##' 
##' 
##' @family Tools
##' 
##'   
##' @examples
##' library(ncdf4)
##' 
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
##' 
##' @return
##' 
##' A \code{vector}
##' 
##' @family Tools
##' 
##' @examples
##' library(ncdf4)
##' 
##' @export
##' 
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
##' 
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
##' 
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
##' 
##' @return
##' 
##' A \code{boolean}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
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
##' @description Extract a proper formatted legend from the attributes stored
##' in a \code{data.frame} 
##' 
##' @param df a \code{data.frame}
##' @param this.dim a \code{character}, the dimension to filter
##' @param n.dim.level a \code{numeric}, the max. number of level to keep. If 
##' \code{NULL}, no filtering occur
##' 
##' @return
##' 
##' A \code{data.frame}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' 
##' @importFrom rlang sym `!!`
##' @export


filter_dim <- function(df, this.dim, 
                       n.dim.level, list_dim = NULL) {
  if (any(colnames(df) == this.dim) & (!is.null(n.dim.level) | !is.null(list_dim))) {
    doFilter <- FALSE
    if (is.null(list_dim)) {
      list_dim <- unique(df[, this.dim])
      n_list_dim <- length(list_dim)
      if (n_list_dim > n.dim.level) {
        doFilter <- TRUE
        list_dim <- c(list_dim[floor(seq(1, n_list_dim, length.out = n.dim.level))])
      }
    } else {
      list_dim <- list_dim[list_dim %in% unique(df[, this.dim])]
      doFilter <- TRUE
    }
    if (doFilter) {
      df <- 
        df %>% 
        filter(!!sym(this.dim) %in% list_dim)
      if (length(list_dim) == 1) {
        df <- 
          df %>% 
          select(-sym(this.dim))
        list_dimname <- attr(df, "dimname")
        attr(df, "dimname") <- list_dimname[-which(list_dimname == this.dim)]
      }
    }
  }
  df
}




# var_with_dim ---------------------------------------------------------
##' @name var_with_dim
##' @author Remi Lemaire-Patin
##' 
##' @title Filter a \code{data.frame} dimension
##' 
##' @description Extract a proper formatted legend from the attributes stored
##' in a \code{data.frame} 
##' 
##' @param df a \code{data.frame}
##' @param this.dim a \code{character}, the dimension to filter
##' @param n.dim.level a \code{numeric}, the max. number of level to keep. If 
##' \code{NULL}, no filtering occur
##' 
##' @return
##' 
##' A \code{data.frame}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' 
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
##' @title Filter a \code{data.frame} dimension
##' 
##' @description Extract a proper formatted legend from the attributes stored
##' in a \code{data.frame} 
##' 
##' @param df a \code{data.frame}
##' @param this.dim a \code{character}, the dimension to filter
##' @param n.dim.level a \code{numeric}, the max. number of level to keep. If 
##' \code{NULL}, no filtering occur
##' 
##' @return
##' 
##' A \code{data.frame}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' 
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
##' @title Filter a \code{data.frame} dimension
##' 
##' @description Extract a proper formatted legend from the attributes stored
##' in a \code{data.frame} 
##' 
##' @param df a \code{data.frame}
##' @param this.dim a \code{character}, the dimension to filter
##' @param n.dim.level a \code{numeric}, the max. number of level to keep. If 
##' \code{NULL}, no filtering occur
##' 
##' @return
##' 
##' A \code{data.frame}
##' 
##' @family Tools
##'   
##' @examples
##' library(ncdf4)
##' 
##' @export
##' @importFrom foreach foreach `%do%`
var_with_same_dim <- function(x, this.var) {
  list.dim <- list_dim_allvar(x[[1]])
  aimed.dim <- list.dim[[this.var]]
  tmp <- sapply(list.dim, function(this.dim){
    length(aimed.dim) == length(this.dim) &&
      all(aimed.dim %in% this.dim)
  })
  list.names <- names(tmp)[tmp]
  str_subset(list.names, this.var, negate = TRUE)
}get_time_range <- function(musica.list) {
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
get_6month <- function(musica.list) {
  this.range <- get_time_range(musica.list)
  this.min <- this.range[1]
  this.max <- min(this.range[2], this.range[1] + 24*3600*180)
  c(this.min, this.max)
}
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
