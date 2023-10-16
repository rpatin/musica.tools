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
  
  if (dimname == "time") {
    return_values <- format_time(return_values, date0 = x$dim$time$units)
  }
  
  if (dimname == "nspecies") {
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
##' @export


format_time <- function(time, date0) {
  date0 = ymd_hms(date0)
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