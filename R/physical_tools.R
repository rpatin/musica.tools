# lat_heat_vap ---------------------------------------------
# from lat_heat_vap in mo_musica_utils.f90
##' @name lat_heat_vap
##' @author Remi Lemaire-Patin
##' 
##' @title latent heat of vaporisation
##' 
##' @description This function computes latent heat of vaporisation (J kg-1) 
##' from air temperature (K). From Monteith J.L., 1973, Principles of Env. 
##' Physics, Arnold.
##' 
##' 
##' @param tk_air a \code{numeric}, temperature in Kelvin
##' 
##' @return
##' 
##' A \code{numeric}
##' 
##' 
##' 
##' 
##' @family Tools
##' 
##'   
##' @examples
##' lat_heat_vap(300)
##' 
##' @export
##' 
##' 

lat_heat_vap <- function(tk_air) {
  # latent heat of evaporation at 298  K (J kg-1)
  lat_heat_vap_298K <-  2.5008e+06
  # slope
  slope  <- 0.00237e+6
  # freezing point (K)
  tp_00 <- 273.15
  # standard room temperature (25 degC) (K)
  tp_298k <- tp_00 + 25 # 298.15
  
  lat_heat_vap_298K - slope*(tk_air - tp_298k)
}

# mol2kg_water_ratio ---------------------------------------------
##' @name mol2kg_water_ratio
##' @author Remi Lemaire-Patin
##' 
##' @title translate water vapour mixing ratio
##' 
##' @description This function computes water vapour mixing ratio from mol/mol 
##' to kg/kg
##' 
##' 
##' @param wair a \code{numeric}, mixing ratio in mol/mol
##' 
##' @return
##' 
##' A \code{numeric}
##' 
##' @family Tools
##' 
##'   
##' @examples
##' mol2kg_water_ratio(0.03)
##' 
##' @export
##' 
##' 

mol2kg_water_ratio <- function(wair) {
  mol_weight_air <- 28.966e-03
  mol_weight_h2o <- 18.016e-03
  wair*mol_weight_h2o/mol_weight_air
}

# e_air ---------------------------------------------
# from e_air in mo_musica_utils.f90
##' @name e_air
##' @author Remi Lemaire-Patin
##' 
##' @title Computes air water vapour pressure (Pa) 
##' 
##' @description This function computes air water vapour pressure (Pa) from air
##'   water vapour  mixing ratio (kg kg-1) and air pressure (Pa).
##' 
##' 
##' @param q_air a \code{numeric}, mixing ratio in kg/kg
##' @param pressure a \code{numeric}, air pressure in Pa
##' 
##' @return
##' 
##' A \code{numeric}
##' 
##' @family Tools
##' 
##'   
##' @examples
##' e_air(0.01, pressure = 1e5)
##' 
##' @export
##' 
##' 

e_air <- function(q_air, pressure) {
  mol_weight_air <-  28.966e-03
  mol_weight_h2o <-  18.016e-03
  c0 <- mol_weight_h2o/mol_weight_air
  pressure * q_air/(c0)
}

# e_air_sat ---------------------------------------------
# from e_air_sat in mo_musica_utils.f90
##' @name e_air_sat
##' @author Remi Lemaire-Patin
##' 
##' @title Computes saturated air water vapour pressure (Pa)
##'
##' @description This function computes saturated air water vapour pressure (Pa)
##'   from air temperature (K). This empirical formula is accurate at 0.1%
##'   between -25 degC and +35 degC The main advantage is that it it is
##'   invertible.
##' 
##' 
##' @param tk_air a \code{numeric}, temperature in Kelvin
##' 
##' @return
##' 
##' A \code{numeric}
##' 
##' @family Tools
##' 
##'   
##' @examples
##' e_air_sat(300)
##' 
##' @export
##' 
##' 

e_air_sat <- function(tk_air) {
  
  eta  = 6.113e+2
  alfa = 4.137e+1
  beta = 1.420e+0
  psi  = 3.212e-2
  mu   = 3.186e-4
  
  tp_00 <-  273.15
  diff_tk <-  tk_air - tp_00
  eta + (alfa + (beta + (psi + mu*diff_tk)*diff_tk)*diff_tk)*diff_tk
}


# convert.units ---------------------------------------------
##' @name convert.units
##' @author Remi Lemaire-Patin
##' 
##' @title Standard unit conversion
##'
##' @description This function converts values between specific set of units 
##' (see Details)
##' 
##' 
##' @param values a \code{numeric}, values to be converted
##' @param from a \code{character}, origin units
##' @param to a \code{character}, aimed units
##' @param dt (\emph{optional}) a \code{numeric}, duration of a time step (in s)
##'   required for some unit conversion
##' @return
##' 
##' A \code{numeric}
##' 
##' @details
##' The corresponding conversion are available
##' \itemize{
##'   \item \code{from = "mmol/m2/dt" ; to = "kg/m2/s"}. Require argument \code{dt}
##' }
##' 
##' 
##' @family Tools
##' 
##'   
##' @examples
##' convert.units(900, from = "mmol/m2/dt", to = "kg/m2/s", dt = 1800)
##' 
##' @export
##' 
##' 

convert.units <- function(values, from, to, dt = NULL) {
  if (from == "mmol/m2/dt" &
      to == "kg/m2/s") {
    stopifnot(!is.null(dt))
    mol_weight_h2o = 18.016e-03 #kg/mol
    values <- values/dt*1e-3*mol_weight_h2o
  }
  return(values)
}