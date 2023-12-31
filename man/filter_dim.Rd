% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{filter_dim}
\alias{filter_dim}
\title{Filter a \code{data.frame} dimension}
\usage{
filter_dim(df, this.dim, n.dim.level, list_dim = NULL)
}
\arguments{
\item{df}{a \code{data.frame}}

\item{this.dim}{a \code{character}, the dimension to filter}

\item{n.dim.level}{a \code{numeric}, the max. number of level to keep. If 
\code{NULL}, no filtering occur}
}
\value{
A \code{data.frame}
}
\description{
Extract a proper formatted legend from the attributes stored
in a \code{data.frame}
}
\examples{
library(ncdf4)

}
\seealso{
Other Tools: 
\code{\link{.fun_testIfIn}()},
\code{\link{attr_legend}()},
\code{\link{e_air_sat}()},
\code{\link{e_air}()},
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
