\name{smhi_opendata_assembler}

\alias{smhi_opendata_assembler}

\title{download SMHI in-situ data and format the output}

\description{
The data are downloaded by using \code{\link{gibson_smhi_opendata}} that is linked to the SMHI Open Data Meteorological Observations REST API (\link{https://opendata-download-metobs.smhi.se}).

The output is formatted and presented as a data frame where every row includes all the metadata and the observations for a specific source/timestamp.

The function has been implemented and tested to download hourly data.
}

\usage{
smhi_opendata_assembler(frost_oldElementCodes=NULL,
                        version="1.0",
                        station_set_all=T,
                        latest_hour=T,
                        formatOUT="\%Y-\%m-\%dT\%H",
                        coords=data.frame(x="lon",y="lat",
                                          proj4="+proj=longlat +datum=WGS84",
                                          stringsAsFactors=F),
                        na.rm=T,
                        url.show=F,
                        verbose=F) 
}

\arguments{

\item{frost_oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). See Details for more information or \link{https://frost.met.no/elementtable}.}

\item{version}{character string with the API version \link{https://opendata.smhi.se/apidocs/metobs/version.html}}

\item{station_set_all}{logical. If TRUE then the station set is set to ''all'' \link{https://opendata.smhi.se/apidocs/metobs/stationSet.html}. Note that according to the SMHI documentation station-set set to all is only possible in combination with ''latest hour'' as period.}

\item{latest_hour}{logical. If TRUE then the hourly data of the latest hour are downloaded. In fact, there is a predefined query to do that, see the JSON example at \link{https://opendata.smhi.se/apidocs/metobs/data.html}.}

\item{formatOUT}{charater string specifying the date-time format of the output timestamps (see \code{\link{strptime}} help page)}

\item{na.rm}{logical, if TRUE remove NAs from the output}

\item{url.show}{logical, if TRUE the urls are shown to the user}

\item{coords}{data frame specifying the coordinate reference systems (as proj4 strings \link{http://proj4.org/}) to include in the output together with the names of the easting and northing coordinate.}

\item{verbose}{logical, if TRUE verbose mode is activated}

}

\details{
\code{frost_oldElementCodes} abbreviations are listed at \link{https://frost.met.no/elementtable}. Not all abbreviations have been implemented (see \code{\link{frost_translate_oldElementCodes}}).

The coordinate reference systems (CRS) are specified by means of proj4 strings \link{http://proj4.org/}. A list of proj4 string can be found at \link{http://spatialreference.org/}.
Further details on the coordinate conversion can be found at \code{\link{crs_transform}}.
It is possible to specify more than one CRS to be included in the output.
The coordinate transformation is based on functions available through the packages \code{\link{sp}}, \code{\link{rgdal}}.

The observation timestamp (UTC) mark the end of the aggregation period.

The quality code  can be ''either G (green), Y (yellow) or R (red) depending on the quality of the value'', see \link{https://opendata.smhi.se/apidocs/metobs/schemas.html#specific} under metObsSampleValueType and metObsIntervalValueType.
}

\value{
A data frame is returned.
The number of columns depends on the input parameters.
Each row includes all the metadata and observed values for a single sourceId/timestamp.
 
The column names are: \code{timestamp} (end of the aggregation period), \code{sourceId} (station), \code{easting coordinate} for the 1st CRS, \code{northing coordinate} for the 1st CRS, \code{easting coordinate} for the 2nd CRS, \code{northing coordinate} for the 2nd CRS, ... , \code{z} (elevation a.m.s.l.), \code{variable1}, \code{variable1_qcode}, \code{variable2}, \code{variable2_qcode}, ...
}

\references{ }

\author{ Cristian Lussana }

\note{The R-packages  \code{\link{sp}} and \code{\link{rgdal}} are needed for the coordinate conversions. SMHI data licence \link{https://opendata.smhi.se/apidocs/metobs/index.html#license}.}

\seealso{ \code{\link{gibson_smhi_opendata}} \code{\link{crs_transform}} }

\examples{

# load libraries
library(gibson)
#
res<-smhi_opendata_assembler(frost_oldElementCodes=c("FF","PR","TA","DD","RR_1","FG","UU"))
}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{download, SMHI, Sweden, opendata.smhi.se, format, output }

