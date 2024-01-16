#' Example dataset from Le Bray
#'
#' A netcdf containing musica model output for 2 weeks at site Le Bray from 
#' 1st to 15th june 2006.
#'
#' @format A netcdf with 720 time steps and 35 variables:
#' \describe{
#'   \item{relative_height}{Height relative to vegetation height}
#'   \item{latitude}{Latitude (degrees)}
#'   \item{longitude}{Longitude (degrees)}
#'   \item{veget_height_top}{Maximum vegetation height (m)}
#'   \item{layer_thickness}{Thickness of each air layer}
#'   \item{z_soil}{Soil depth (m)}
#'   \item{dz_soil}{Soil layer thickness (m)}
#'   \item{porosity_soil}{Soil total porosity (m^3/m^3)}
#'   \item{h_soil}{Soil water potential (MPa)}
#'   \item{T_soil}{Soil temperature (K)}
#'   \item{w_soil}{Soil water content (m^3/m^3)}
#'   \item{co2_soil}{Soil CO2 mixing ratio (ppmv)}
#'   \item{q_h2o_soil}{Soil water flux (kg/m^2/s)}
#'   \item{evap_soil}{Soil evaporation flux (kg/m^2/s)}
#'   \item{h_canopy}{Xylem water potential of canopy (MPa)}
#'   \item{root_uptake}{Root water uptake (kg/m^2/s)}
#'   \item{Tair_z}{Air temperature (K)}
#'   \item{wair_z}{Air humidity (mol/mol)}
#'   \item{CO2air_z}{Air co2 concentration (ppmv)}
#'   \item{wind_z}{Wind speed (m/s)}
#'   \item{Qh}{Sensible heat flux at reference level (W/m^2)}
#'   \item{Qle}{Latent heat flux at reference level (W/m^2)}
#'   \item{NEE}{Net Ecosystem Exchange (gC/m^2/s)}
#'   \item{Rnet}{Net radiation (shortwave + longwave) at reference level (W/m^2)}
#'   \item{LWnet}{Net longwave radiation at reference level (W/m^2)}
#'   \item{SWnet}{Net shortwave radiation at reference level (W/m^2)}
#'   \item{Wind_frict}{Surface friction velocity at reference level (m/s)}
#'   \item{Evap}{Total evapotranspiration (kg/m^2/s)}
#'   \item{Qh_soil}{Soil sensible heat flux (W/m^2)}
#'   \item{Qle_soil}{Soil latent heat flux (W/m^2)}
#'   \item{Rnet_soil}{Soil net radiation (W/m^2)}
#'   \item{Qco2_soil}{Soil net CO2 flux (gC/m^2/s)}
#'   \item{Qg}{Soil heat flux (W/m^2)}
#'   \item{transpir}{Transpiration rate (kg/m^2/s)}
#'   \item{gpp}{Gross primary productivity (umol/m2/s)}
#' }
#' @examples
#' x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
#' 
