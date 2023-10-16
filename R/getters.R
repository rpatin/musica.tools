# get_variable ---------------------------------------------------------
##' @name get_variable
##' @author Remi Lemaire-Patin
##' 
##' @title Get a variable from a \code{ncdf4} object
##' 
##' @description This function extract a variable from a \code{ncdf4} object and
##' format it in a nice \code{data.frame}.
##' 
##' 
##' @param x a \code{ncdf4} object
##' 
##' @return
##' 
##' A \code{data.frame}
##' 
##' 
##' @details 
##' 
##' \describe{
##' Additional Description
##' }
##' 
##' 
##' @family getters
##' 
##'   
##' @examples
##' library(ncdf4)
##' 
##' @importFrom dplyr filter
##' @export
##' 
##' 

get_variable <- function(x, varname, time_range) {
  time_range <- .check_get_variable(
    x = x,
    varname = varname,
    time_range = time_range
  )
  dfdim <- get_dim_info(x)
  list_dimname <- 
    lapply(x$var[[varname]]$dimids,           
           function(thisid) {
             dfdim$dimname[which(dfdim$id == thisid)]
           })
  list_dimvalue <-
    lapply(list_dimname, 
           function(this_dimname) {
             get_dim_value(x, this_dimname)
           })
  
  df <- expand.grid(list_dimvalue)
  colnames(df) <- unlist(list_dimname)
  matvalue <- ncvar_get(x, varname)
  df[ , varname] <- as.vector(matvalue)
  which.NA <- which(df[ , varname] == -9999)
  if (length(which.NA) > 0) {
    df[which.NA, varname ] <- NA
  }
  
  attr(df, "units") <- x$var[[varname]]$units
  attr(df, "longname") <- x$var[[varname]]$longname
  if (!is.null(time_range)) {
    df <- 
      df %>% 
      filter(time >= time_range[1],
             time <= time_range[2])
  }
  df
}

.check_get_variable <- function(x, varname, time_range) {
  if (!inherits(x,"ncdf4")) {
    stop("x must be a ncdf4 object")
  }
  if ( length(varname) != 1) {
    stop("varname must be of length 1")
  }
  
  if (!varname %in% names(x$var)) {
    stop("variable ", varname, " not found in x\n
varname must be among ", paste0(names(x$var), collapes = " ; "))
  }
  
  if (missing(time_range)) {
    time_range <- NULL
  } else {
    if (!is.null(time_range)) {
      stopifnot(inherits(time_range,"POSIXct"))
      stopifnot(length(time_range) == 2)
    }
  }
  time_range
}

# get_all_var ---------------------------------------------------------
##' @name get_all_var
##' @author Remi Lemaire-Patin
##' 
##' @title extract all variable from a \code{ncdf4} object
##' 
##' @description This function extract all variable from a \code{ncdf4} object 
##' and format it as a nice list of \code{data.frame}.
##' 
##' 
##' @param x a \code{ncdf4} object
##' 
##' @return
##' 
##' A \code{data.frame}
##' 
##' 
##' @details 
##' 
##' \describe{
##' Additional Description
##' }
##' 
##' 
##' @family getters
##' 
##'   
##' @examples
##' library(ncdf4)
##' 
##' @export
##' 
##' 

get_all_var <- function(x) {
  .check_get_all_var(
    x = x
  )
  list_var <- lapply(names(x$var), 
                     function(this_var){
                       get_variable(x, this_var)
                     })
  names(list_var) <- names(x$var)
  list_var
}

.check_get_all_var <- function(x) {
  if (!inherits(x,"ncdf4")) {
    stop("x must be a ncdf4 object")
  }
}