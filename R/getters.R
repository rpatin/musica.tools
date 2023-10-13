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
##' @export
##' 
##' 

get_variable <- function(x, varname) {
  .check_get_variable(
    x = x,
    varname = varname
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
  attr(df, "units") <- x$var[[varname]]$units
  df
}

.check_get_variable <- function(x, varname) {
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
  NULL
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