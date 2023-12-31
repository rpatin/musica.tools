% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/physical_tools.R
\name{e_air_sat}
\alias{e_air_sat}
\title{Computes saturated air water vapour pressure (Pa)}
\usage{
e_air_sat(tk_air)
}
\arguments{
\item{tk_air}{a \code{numeric}, temperature in Kelvin}
}
\value{
A \code{numeric}
}
\description{
This function computes saturated air water vapour pressure (Pa)
  from air temperature (K). This empirical formula is accurate at 0.1%
  between -25 degC and +35 degC The main advantage is that it it is
  invertible.
}
\examples{
mol2kg_water_ratio(0.03)

}
\seealso{
Other Tools: 
\code{\link{.fun_testIfIn}()},
\code{\link{attr_legend}()},
\code{\link{e_air}()},
\code{\link{filter_dim}()},
\code{\link{format_time}()},
\code{\link{get_dim_info}()},
\code{\link{get_dim_value}()},
\code{\link{lat_heat_vap}()},
\code{\link{mol2kg_water_ratio}()},
\code{\link{var_with_dim}()}
}
\author{
Remi Lemaire-Patin
}
\concept{Tools}
