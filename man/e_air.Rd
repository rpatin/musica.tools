% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/physical_tools.R
\name{e_air}
\alias{e_air}
\title{Computes air water vapour pressure (Pa)}
\usage{
e_air(q_air, pressure)
}
\arguments{
\item{q_air}{a \code{numeric}, mixing ratio in kg/kg}

\item{pressure}{a \code{numeric}, air pressure in Pa}
}
\value{
A \code{numeric}
}
\description{
This function computes air water vapour pressure (Pa) from air
  water vapour  mixing ratio (kg kg-1) and air pressure (Pa).
}
\examples{
mol2kg_water_ratio(0.03)

}
\seealso{
Other Tools: 
\code{\link{.fun_testIfIn}()},
\code{\link{attr_legend}()},
\code{\link{e_air_sat}()},
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
