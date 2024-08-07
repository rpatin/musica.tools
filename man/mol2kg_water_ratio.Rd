% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/physical_tools.R
\name{mol2kg_water_ratio}
\alias{mol2kg_water_ratio}
\title{translate water vapour mixing ratio}
\usage{
mol2kg_water_ratio(wair)
}
\arguments{
\item{wair}{a \code{numeric}, mixing ratio in mol/mol}
}
\value{
A \code{numeric}
}
\description{
This function computes water vapour mixing ratio from mol/mol 
to kg/kg
}
\examples{
mol2kg_water_ratio(0.03)

}
\seealso{
Other Tools: 
\code{\link{.fun_testIfIn}()},
\code{\link{attr_legend}()},
\code{\link{convert.units}()},
\code{\link{e_air}()},
\code{\link{e_air_sat}()},
\code{\link{filter_dim}()},
\code{\link{format_time}()},
\code{\link{get_6month}()},
\code{\link{get_air_level}()},
\code{\link{get_dim_info}()},
\code{\link{get_dim_value}()},
\code{\link{get_soil_level}()},
\code{\link{get_time_range}()},
\code{\link{lat_heat_vap}()},
\code{\link{list_allvar}()},
\code{\link{list_dim_allvar}()},
\code{\link{var_with_dim}()},
\code{\link{var_with_same_dim}()}
}
\author{
Remi Lemaire-Patin
}
\concept{Tools}
