% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary_pdf}
\alias{summary_pdf}
\title{render a pdf report}
\usage{
summary_pdf(
  x,
  filename,
  template,
  list_var,
  add_study_site = FALSE,
  time_range,
  file.width,
  file.height,
  ...
)
}
\arguments{
\item{x}{a \code{ncdf4} object}

\item{filename}{a \code{character}, file name to save the report, without
extension (automatically added)}

\item{template}{\emph{(optional)}, a \code{character}, with preset of
variables that can be exported, see Details.}

\item{list_var}{\emph{(optional, ignored if \code{template} argument was
given)} a \code{character vector} with list of variables to be exported.}

\item{add_study_site}{(\emph{optional}, default \code{FALSE}) a
\code{boolean}, if \code{TRUE} adds a leaflet with location of study site}

\item{time_range}{(\emph{optional}) a \code{POSIXct vector} with two values:
the time range to extract}

\item{file.width}{file width transmitted to \code{pdf()}}

\item{file.height}{file height transmitted to \code{pdf()}}

\item{...}{additional parameters transmitted to \code{\link{ggplot_variable}}}
}
\description{
This function render a markdown report for a selection of 
variables. Report includes static plot for a given selection of variable
}
\examples{
library(ncdf4)
x <- nc_open(system.file("extdata", "musica_out_2006_demo.nc", package = "musica.tools"))
summary_pdf(x, "test_summary", template = "Overview")
file.remove("test_summary.pdf")

}
\seealso{
Other summary: 
\code{\link{summary_markdown}()}
}
\author{
Remi Lemaire-Patin
}
\concept{summary}
