\name{fmi_station_list}

\alias{fmi_station_list}

\title{download FMI station list}

\description{
Download the station list for the FMI in-situ observations.
As far as I know, the station list at the web-page \link{http://en.ilmatieteenlaitos.fi/observation-stations} is the only source that report the station elevation.
}

\usage{
fmi_station_listfunction(file_station_list="/lustre/storeB/project/metkl/senorge2/case/etc/fmi_oper_list_of_stations.html")
}

\arguments{

\item{file_station_list}{file with the station list, derived from \link{http://en.ilmatieteenlaitos.fi/observation-stations}. This is simply copy-and-paste of the web-page taken from the browser.}

}

\details{
The url used by default is \link{http://en.ilmatieteenlaitos.fi/observation-stations}.
}

\value{
A data frame is returned.

The column names are: \code{name} (station name), \code{fmisid} (FMI station ID), \code{wmo} WMO identifier, \code{elevation} (elevation a.m.s.l.), \code{group}, \code{station group}
}

\references{ }

\author{ Cristian Lussana }

\note{ FMI open data licence \link{http://en.ilmatieteenlaitos.fi/open-data-licence}.}

\seealso{ \code{\link{fmi_opendata_assembler}}}

\examples{

# load libraries
library(gibson)
#
stnlist<-fmi_station_listfunction()
}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{download, station list, metadata, FMI, data.fmi.fi }

