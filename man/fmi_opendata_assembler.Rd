\name{fmi_opendata_assembler}

\alias{fmi_opendata_assembler}

\title{download FMI in-situ data and format the output}

\description{
The FMI data are downloaded by using \code{\link{gibson_fmi_opendata}} that is linked to the FMI API (\link{data.fmi.fi}).

The output is formatted and presented as a data frame where every row includes all the metadata and the observations for a specific source/timestamp.

The function has been implemented and tested to download hourly and diurnal data.
}

\usage{
gibson_frost(apiKey=NULL,
             oldElementCodes=NULL,
             timeOffset=0,
             start_date=NULL,
             stop_date=NULL,
             format="%Y-%m-%dT%H:%M",
             formatOUT="%Y-%m-%dT%H",
             spatial_extent=c(19.09,59.3,31.59,70.13),
             stationholders=NULL,
             stationholders.exclude=F,
             WMOonly=F,
             WMOin=T,
             na.rm=T,
             url.show=F,
             coords=data.frame(x="lon",y="lat",
                    proj4="+proj=longlat +datum=WGS84",
                    stringsAsFactors=F),
             url4stnlist="http://en.ilmatieteenlaitos.fi/observation-stations",
             verbose=F)
}

\arguments{

\item{apiKey}{API-key that can be gotten via registration \link{https://ilmatieteenlaitos.fi/rekisteroityminen-avoimen-datan-kayttajaksi}}

\item{oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). See Details for more information or \link{https://frost.met.no/elementtable}. The oldElementCodes are transladed into the FMI observations, that are described at \link{http://en.ilmatieteenlaitos.fi/guidance-to-observations} by \code{\link{fmi_opendata_translate_oldElementCodes}}.}

\item{timeOffset}{numeric number of seconds to add to the original FMI timestamp. It can be used to adapt the timestamp to different time conventions.}

\item{start_date}{character string with the time stamp of the first observation} 

\item{stop_date}{character string with the time stamp of the last observation} 

\item{format}{charater string specifying the date-time format of start_date and stop_date (see \code{\link{strptime}} help page)}

\item{formatOUT}{charater string specifying the date-time format of the output timestamps (see \code{\link{strptime}} help page)}

\item{spatial_extent}{numeric vector of the form c(long_min,long_max,lan_min,lan_max) that identifies a rectangle used to select the data sources. The lower left corner is (long_min,lan_min) the upper right corner is (long_max,lat_max).}

\item{stationholders}{character vector with the names of the station holders. The list of observation stations is available at \link{http://en.ilmatieteenlaitos.fi/observation-stations}. The available station holders are: 'Weather', 'Precipitation', 'Radar', 'Sounding', 'Mast', 'Buoy', 'Air quality', 'Air quality (urban)'.}

\item{stationholders.exclude}{logical, it is used in combination with \code{stationholders}. If FALSE, then the \code{stationholders} list will be used to select those station holders to include in the output. If TRUE, then the \code{stationholders} list will be used to select those station holders NOT to include in the output. Note that it might happen that a station has more than one station holder, in this case if at least one of the specified \code{stationholders} is included within the station holders of a station then the correspondent action (i.e., include/exclude) will be executed.}

\item{WMOonly}{logical, if TRUE then only WMO stations (i.e., having assigned a WMO code) will be returned}

\item{WMOin}{logical, used only if WMOonly=FALSE. if TRUE then WMO stations (i.e., having a WMO code) will be returned otherwise the will not.}

\item{na.rm}{not used}

\item{url.show}{logical, if TRUE the urls are shown to the user}

\item{coords}{data frame specifying the coordinate reference systems (as proj4 strings \link{http://proj4.org/}) to include in the output together with the names of the easting and northing coordinate.}

\item{verbose}{logical, if TRUE verbose mode is activated}

\item{url4stnlist}{character, link to the page with the list of observation stations}

}

\details{
\code{oldElementCodes} abbreviations are listed at \link{https://frost.met.no/elementtable}. Not all abbreviations have been implemented (see \code{\link{frost_translate_oldElementCodes}}).

It is not possible to mix variables with hourly time resolution and variables with daily time resolution.

\code{SourceId} is the FMI-station identifier and the suffix ':fmi' has been added.
The \code{sourceId} is a unique key.

Guidance to the observed values is available at \link{http://en.ilmatieteenlaitos.fi/guidance-to-observations}.
For 'r_1h' hourly precipitation, the timestamp marks the end of the aggregation period.
The other hourly values ('t2m', 'ws_10min', 'p_sea', 'wd_10min', 'wg_10min', 'rh') are actually instantaneous values or 10-minute values referring to the 10-minute interval that ends at minute 00.

For daily (or diurnal) values of: 'rrday' Precipitation is between 0700 UTC and 0700 UTC of the successive day respect to the one reported as the timestamp; 'tmin'  the lowest temperature during two 12 hours period, i.e. between 1900 UTC previous evening and 1900 UTC this evening; 'tmax' the highest temperature during two 12 hours period, i.e. between 1900 UTC previous evening and 1900 UTC this evening; 'tday' the average temperature based usually on 4 or 8 observations per day.

}

\value{
A data frame is returned.
The number of columns depends on the input parameters.
Each row includes all the metadata and observed values for a single sourceId/timestamp.
 
The column names are: \code{timestamp} (end of the aggregation period), \code{sourceId} (station:sensor), \code{easting coordinate} for the 1st CRS, \code{northing coordinate} for the 1st CRS, \code{easting coordinate} for the 2nd CRS, \code{northing coordinate} for the 2nd CRS, ... , \code{z} (elevation a.m.s.l.), \code{variable1}, \code{variable1_qcode}, \code{variable2}, \code{variable2_qcode}, ...
}

\references{ }

\author{ Cristian Lussana }

\note{ FMI open data licence \link{http://en.ilmatieteenlaitos.fi/open-data-licence}.}

\seealso{ \code{\link{fmi_opendata_assembler}}}

\examples{

# load libraries
library(gibson)
#
apiKey<-put_your_apiKey_here
#--------------------------------------------------------------------
# get hourly data on a real-time basis
fmi<-fmi_opendata_assembler(apiKey=apiKey,oldElementCodes=c("RR_1","TA","UU"),timeOffset=0,start_date=NULL,stop_date=NULL,format="%Y-%m-%dT%H:%M",formatOUT="%Y-%m-%dT%H",spatial_extent=c(19.09,59.3,31.59,70.13),stationholders=NULL,stationholders.exclude=F,WMOonly=F,WMOin=T,na.rm=T,url.show=F,coords=data.frame(x="lon",y="lat",proj4="+proj=longlat +datum=WGS84",stringsAsFactors=F),url4stnlist="http://en.ilmatieteenlaitos.fi/observation-stations",verbose=F)
#--------------------------------------------------------------------
# get daily data on a real-time basis
fmi<-fmi_opendata_assembler(apiKey=apiKey,oldElementCodes=c("RR","TAM"),timeOffset=86400,start_date=NULL,stop_date=NULL,format="%Y-%m-%dT%H:%M",formatOUT="%Y-%m-%d",spatial_extent=c(19.09,59.3,31.59,70.13),stationholders=NULL,stationholders.exclude=F,WMOonly=F,WMOin=T,na.rm=T,url.show=F,coords=data.frame(x="lon",y="lat",proj4="+proj=longlat +datum=WGS84",stringsAsFactors=F),url4stnlist="http://en.ilmatieteenlaitos.fi/observation-stations",verbose=F)
}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{download, FMI, data.fmi.fi }

