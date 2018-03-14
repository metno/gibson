\name{frost_assembler}

\alias{frost_assembler}

\title{download MET Norway in-situ data and format the output}

\description{
The data are downloaded by using \code{\link{gibson_frost}} that is linked to the Frost API (\link{frost.met.no}).

The output is formatted and presented as a data frame where every row includes all the metadata and the observations for a specific source/timestamp.

The function has been implemented and tested to download hourly and diurnal data.
}

\usage{
frost_assembler(client_id=NULL,
                oldElementCodes=NULL,
                start_date=NULL,
                stop_date=NULL,
                format="%Y-%m-%dT%H:%M",
                formatOUT="%Y-%m-%dT%H",
                countries="NO",
                spatial_extent=c(4,34,54,72),
                stationholders=NULL,
                stationholders.exclude=F,
                WMOonly=F,
                na.rm=T,
                url.show=F,
                coords=data.frame(x="lon",y="lat",
                       proj4="+proj=longlat +datum=WGS84",
                       stringsAsFactors=F),
                verbose=F
                )
}

\arguments{

\item{client_id}{character string with the API Client ID \link{https://frost.met.no/auth/requestCredentials.html}}

\item{oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). See Details for more information or \link{https://frost.met.no/elementtable}.}

\item{start_date}{character string with the time stamp of the first observation} 

\item{stop_date}{character string with the time stamp of the last observation} 

\item{format}{charater string specifying the date-time format of start_date and stop_date (see \code{\link{strptime}} help page)}

\item{formatOUT}{charater string specifying the date-time format of the output timestamps (see \code{\link{strptime}} help page)}

\item{countries}{character vector with the abbreviations of the countries. Only data sources located in those countries will be returned.}

\item{spatial_extent}{numeric vector of the form c(long_min,long_max,lan_min,lan_max) that identifies a rectangle used to select the data sources. The lower left corner is (long_min,lan_min) the upper right corner is (long_max,lat_max).}

\item{stationholders}{character vector with the names of the station holders}

\item{stationholders.exclude}{logical, it is used in combination with stationholders If FALSE, then the stationholder list will be used to select the station holders to include in the output.  If TRUE, then the stationholder list will be used to select the station holders NOT to include in the output.}

\item{WMOonly}{logical, if TRUE then only WMO stations (i.e., having a WMO code will be returned}

\item{na.rm}{logical, if TRUE remove NAs from the output}

\item{url.show}{logical, if TRUE the urls are shown to the user}

\item{coords}{data frame specifying the coordinate reference systems (as proj4 strings \link{http://proj4.org/}) to include in the output together with the names of the easting and northing coordinate.}

\item{verbose}{logical, if TRUE verbose mode is activated}

}

\details{
\code{oldElementCodes} abbreviation meanings are: \\"RR_1\\" hourly total precipitation, \\"TA\\" two-metre air temperature / hourly sampling rate, \\"TAM\\" daily mean temperature (from 1800 UTC previous day to 1800 UTC observation day), \\"TAMRR\\" daily mean temperature (from 0600 UTC previous day to 0600 UTC observation day), \\"RR\\" daily total precipitation (from 0600 UTC previous day to 0600 UTC observation day). 

It is not possible to mix variables with hourly time resolution and variables with daily time resolution.

\code{SourceId} is the data source identifier of an observation. From the documentation on frost.met.no:  \\"The ID(s) of the data sources to get time series for as a comma-separated  list of Frost API station IDs: SN<int>[:<int>|all]  (e.g. SN18700, SN18700:0, or SN18700:all). 0 is the main sensor and x≥1 is a  parallel sensor.\\"

The coordinate reference systems (CRS) are specified by means of proj4 strings \link{http://proj4.org/}. A list of proj4 string can be found at \link{http://spatialreference.org/}.
Further details on the coordinate conversion can be found at \code{\link{crs_transform}}.
It is possible to specify more than one CRS to be included in the output.

The output variables have the same names of the \code{oldElementCodes} and the KDVH quality codes are also returned.
The quality code meanings are available at this link \link{https://frost.met.no/observations/availableQualityCodes/v0.jsonld?lang=nb-NO}.

The observation timestamp (UTC) mark the end of the aggregation period.
}

\value{
A data frame is returned.
The number of columns depends on the input parameters.
Each row includes all the metadata and observed values for a single sourceId/timestamp.
 
The column names are: \code{timestamp} (end of the aggregation period), \code{sourceId} (station:sensor), \code{easting coordinate} for the 1st CRS, \code{northing coordinate} for the 1st CRS, \code{easting coordinate} for the 2nd CRS, \code{northing coordinate} for the 2nd CRS, ... , \code{z} (elevation a.m.s.l.), \code{variable1}, \code{variable1_qcode}, \code{variable2}, \code{variable2_qcode}, ...


}

\references{ }

\author{ Cristian Lussana }

\note{ }

\seealso{ \code{\link{gibson_frost}} \code{\link{crs_transform}} }

\examples{

# load libraries
library(gibson)
#
auth<-put_your_ClientID_here
# get hourly precipitation and temperature for all Norwegian stations, add coordinates
#  transformed over several CRSs
d<-frost_assembler(client_id=auth,oldElementCodes=c("RR_1","TA"),start_date="2018-03-08T01:00",stop_date= "2018-03-10T01:00",format="%Y-%m-%dT%H:%M",formatOUT="%Y-%m-%dT%H",countries="NO",spatial_extent=c(4,34,54,72),stationholders=NULL,stationholders.exclude=F,WMOonly=F,na.rm=T,url.show=T,coord=data.frame(x=c("lon","x_lcc","x_utm33","x_laea"),y=c("lat","y_lcc","y_utm33","y_laea"),proj4=c("+proj=longlat +datum=WGS84","+proj=lcc +lat_0=48 +lon_0=8 +lat_1=48  +lat_2=48 +a=6371229 +no_defs","+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0","+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"),stringsAsFactors=F),verbose=T)

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{download, MET Norway, frost.met.no, format, output }

