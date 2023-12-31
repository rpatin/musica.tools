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
##' @importFrom dplyr filter rename
##' @export
##' 
##' 

get_variable <- function(x, varname, time_range, return.colnames = FALSE) {
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
  
  if (return.colnames) {
    return(unlist(list_dimname))
  }
  
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
  
  # rename dev_jerome column names
  old_conversion <- 
    c("n_time" = "time",
      "n_pts_terre" = "nland",
      "n_air_layer" = "nair",
      "n_soil_layer" = "nsoil",
      "n_veg_layer" = "nveg",
      "n_species_max" = "nspecies",
      "n_leaf_age" = "nleafage")
  for (this.old in names(old_conversion)) {
    if (any(colnames(df) == this.old)) {
      df <- 
        rename(df, !!sym(old_conversion[this.old]) := sym(this.old))
    }
  }
  
  attr(df, "units") <- x$var[[varname]]$units
  attr(df, "longname") <- x$var[[varname]]$longname
  attr(df, "var") <- varname
  attr(df, "ndim") <- ncol(df) - 1
  attr(df, "dimname") <- unlist(list_dimname)
  attr(df, "nvar") <- 1
  attr(df, "models") <- NULL
  
  if ("nsoil" %in% list_dimname & 
      !(varname %in% c("z_soil", "dz_soil"))) {
    if ("z_soil" %in% names(x$var)) {
      attr(df, "z_soil") <- 
        get_variable(x, "z_soil") 
    }
    if ("dz_soil" %in% names(x$var)) {
      attr(df, "dz_soil") <- 
        get_variable(x, "dz_soil")
    }
  }
  
  if (("nair" %in% list_dimname |
       "nveg" %in% list_dimname) & 
      !(varname %in% c("relative_height", "layer_thickness"))) {
    if ("relative_height" %in% names(x$var)) {
      attr(df, "relative_height") <- 
        get_variable(x, "relative_height")
    }
    if ("veget_height_top" %in% names(x$var)) {
      attr(df, "veget_height_top") <- 
        get_variable(x, "veget_height_top")
    }
    if ("layer_thickness" %in% names(x$var)) {
      attr(df, "layer_thickness") <- 
        get_variable(x, "layer_thickness") 
    }
  }
  
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


# get_two_variables ---------------------------------------------------------
##' @name get_two_variables
##' @author Remi Lemaire-Patin
##' 
##' @title Get two variable from a \code{ncdf4} object
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
##' @importFrom dplyr filter full_join
##' @export
##' 
##' 

get_two_variables <- function(x, varnames, time_range) {
  time_range <- .check_get_two_variables(
    x = x,
    varnames = varnames,
    time_range = time_range
  )
  df1 <- get_variable(x, varnames[1], time_range)
  df2 <- get_variable(x, varnames[2], time_range)
  df <- full_join(df1, df2)
  attr(df, "var") <- c(attr(df1, "var"), attr(df2, "var"))
  attr(df, "units") <- c(attr(df1, "units"), attr(df2, "units"))
  attr(df, "longname") <- c(attr(df1, "longname"), attr(df2, "longname"))
  attr(df, "var") <- c(attr(df1, "var"), attr(df2, "var"))
  attr(df, "nvar") <- 2
  attr(df, "z_soil") <- attr(df1, "z_soil")
  attr(df, "dz_soil") <- attr(df1, "dz_soil")
  attr(df, "relative_height") <- attr(df1, "relative_height")
  attr(df, "veget_height_top") <- attr(df1, "veget_height_top")
  attr(df, "layer_thickness") <- attr(df1, "layer_thickness")
  df
}

.check_get_two_variables <- function(x, varnames, time_range) {
  if (!inherits(x,"ncdf4")) {
    stop("x must be a ncdf4 object")
  }
  if ( length(varnames) != 2) {
    stop("varnames must be of length 2")
  }
  
  time_range <- .check_get_variable(x, varnames[1], time_range)
  time_range <- .check_get_variable(x, varnames[2], time_range)
  dfdim <- get_dim_info(x)
  list_dimname1 <- 
    lapply(x$var[[varnames[1]]]$dimids,           
           function(thisid) {
             dfdim$dimname[which(dfdim$id == thisid)]
           })
  list_dimname2 <- 
    lapply(x$var[[varnames[2]]]$dimids,           
           function(thisid) {
             dfdim$dimname[which(dfdim$id == thisid)]
           })
  if (!all(unlist(list_dimname1) == unlist(list_dimname2))) {
    stop("Variables ", varnames[1], " and ", varnames[2], " have different dimensions. \n",
         varnames[1], ": ", paste0(unlist(list_dimname1), collapse = " ; "), "\n",
         varnames[2], ": ", paste0(unlist(list_dimname2), collapse = " ; "))
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


# get_variable_comparison ---------------------------------------------------------
##' @name get_variable_comparison
##' @author Remi Lemaire-Patin
##' 
##' @title Get a variable from a list of \code{ncdf4} object
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
##' @importFrom dplyr filter select
##' @export
##' 
##' 

get_variable_comparison <- function(x, this_var, time_range = NULL, 
                                    n.soil.level = NULL, list.soil.level = NULL, 
                                    n.air.level = NULL,  list.air.level = NULL, 
                                    n.species.level = NULL,  list.species.level = NULL, 
                                    n.veg.level = NULL,  list.veg.level = NULL, 
                                    n.leafage.level = NULL,  list.leafage.level = NULL,
                                    diffmodels = FALSE) {
  
  args <- .check_get_variable_comparison(
    this_var,
    diffmodels
  )
  for (argi in names(args)) { 
    assign(x = argi, value = args[[argi]]) 
  }
  rm(args)
  list.df <- lapply(seq_along(x), function(i){
    tmp.try <- try({
      if (length(this_var) == 1) {
        tmp <- get_variable(x[[i]], this_var, time_range = time_range) 
      } else {
        tmp <- get_two_variables(x[[i]], this_var, time_range = time_range) 
      }
      tmp <- 
        tmp %>% 
        filter_dim("nsoil", n.soil.level, list_dim = list.soil.level) %>% 
        filter_dim("nair",  n.air.level, list_dim = list.air.level)  %>% 
        filter_dim("nspecies",  n.species.level, list_dim = list.species.level) %>% 
        filter_dim("nveg",  n.veg.level, list_dim = list.veg.level) %>% 
        filter_dim("nleafage",  n.leafage.level, list_dim = list.leafage.level) 
    })
    if (!inherits(tmp.try, "try-error")) {
      tmp$models <- names(x)[i]
      return(tmp)
    } else {
      return(NULL)
    }
  }) 
  df <- list.df %>% 
    do.call('rbind', .) 
  attr(df, "var") <- this_var
  attr(df, "units") <- attr(list.df[[1]], "units")
  attr(df, "longname") <- attr(list.df[[1]], "longname")
  attr(df, "dimname") <- attr(list.df[[1]], "dimname")
  attr(df, "ndim") <- length(attr(df, "dimname"))
  attr(df, "nvar") <- ncol(df) - attr(df, "ndim")
  attr(df, "models") <- names(x) 
  attr(df, "z_soil") <- attr(list.df[[1]], "z_soil")
  attr(df, "dz_soil") <- attr(list.df[[1]], "dz_soil")
  attr(df, "relative_height") <- attr(list.df[[1]], "relative_height")
  attr(df, "veget_height_top") <- attr(list.df[[1]], "veget_height_top")
  attr(df, "layer_thickness") <- attr(list.df[[1]], "layer_thickness")
  if (diffmodels) {
    df <- 
      df %>% 
      pivot_wider(names_from = "models",
                  values_from = sym(this_var))
    df$diff <- 
      df[, attr(df, "models")[1], drop = TRUE] -
      df[, attr(df, "models")[2], drop = TRUE]
    df <- df[,-which(colnames(df) %in% attr(df, "models"))]
    attr(df, "var") <- "diff"
    
  } else {
    attr(df, "dimname") <- c(attr(df, "dimname"), "models")
    attr(df, "ndim") <- length(attr(df, "dimname"))
  }
  df
}

.check_get_variable_comparison <- function(this_var, diffmodels) {
  .fun_testIfIn(length(this_var), values = c(1,2))
  
  if (length(this_var) > 1) {
    diffmodels <- FALSE 
  } else {
    if (missing(diffmodels) || is.null(diffmodels)) {
      diffmodels <- FALSE
    }
  }
  list(
    this_var = this_var,
    diffmodels = diffmodels
  )
}
