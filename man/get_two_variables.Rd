% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getters.R
\name{get_two_variables}
\alias{get_two_variables}
\title{Get two variable from a \code{ncdf4} object}
\usage{
get_two_variables(x, varnames, time_range)
}
\arguments{
\item{x}{a \code{ncdf4} object}
}
\value{
A \code{data.frame}
}
\description{
This function extract a variable from a \code{ncdf4} object and
format it in a nice \code{data.frame}.
}
\details{
\describe{
Additional Description
}
}
\examples{
library(ncdf4)

}
\seealso{
Other getters: 
\code{\link{get_all_var}()},
\code{\link{get_variable_comparison}()},
\code{\link{get_variable}()}
}
\author{
Remi Lemaire-Patin
}
\concept{getters}
