\name{gibson_fmi_opendata}

\alias{gibson_fmi_opendata}

\title{get FMI in-situ data}

\description{
Download metadata and observations from the FMI-Finnish Meteorological Institute's open data API (\link{data.fmi.fi}).
In particular, this function has been implemented and tested to download hourly and diurnal data, though it can be used for other aggregation intervals too.
The FMI API documentation is available at \link{https://en.ilmatieteenlaitos.fi/open-data}.

The FMI Open data WFS services is described at \link{http://en.ilmatieteenlaitos.fi/open-data-manual-fmi-wfs-services}.

A single observed value returned by \code{gibson_fmi_opendata} can be univocally identified by the union of several pieces of information, such as (for our purposes): \code{sourceId}, \code{oldElementCodes}, \code{timestamp}.
The \code{sourceId} identifies the station/sensor pair. 
The \code{oldElementCodes} specifies the meteorological or climate element (variable/aggregation pair).
The \code{timestamp} defines the observation timestamp (time standard is Coordinated Universal Time or UTC) as designed by the FMI API developers.

For convenience, it is possible to request the Weather and Climate Elements also by using their old element codes (in use before Frost).

Have a look to \code{\link{fmi_opendata_assembler}} to find examples on how to post-process \code{gibson_fmi_opendata} results in order to obtain the desired output.

}

\usage{
gibson_fmi_opendata(apiKey=NULL,
                    frost_oldElementCodes=NULL,
                    timeResolution=NULL,
                    start_date=NULL,
                    stop_date=NULL,
                    format="\%Y-\%m-\%dT\%H:\%M",
                    spatial_extent=c(19.09,59.3,31.59,70.13),
                    stationholders=NULL,
                    stationholders.exclude=F,
                    WMOonly=F,
                    WMOin=T,
                    try.again=1,
                    sleep_sec=5,
                    na.rm=T,
                    url.show=F,
                    file_station_list="/lustre/storeB/project/metkl/senorge2/case/etc/fmi_oper_list_of_stations.html",
                    url4stnlist="http://en.ilmatieteenlaitos.fi/observation-stations"
                    )
}

\arguments{

\item{apiKey}{API-key that can be gotten via registration \link{https://ilmatieteenlaitos.fi/rekisteroityminen-avoimen-datan-kayttajaksi}}

\item{frost_oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). See Details for more information or \link{https://frost.met.no/elementtable}. The oldElementCodes are transladed into the FMI observations, that are described at \link{http://en.ilmatieteenlaitos.fi/guidance-to-observations} by \code{\link{fmi_opendata_translate_oldElementCodes}}.}

\item{timeResolution}{character vector with the time resolutions (list of ISO-8601 periods, e.g. 'PT06H,PT18H').  Available options are: 'P1D' and 'PT1H'. \link{https://frost.met.no/reference#!/observations/timeSeries}}

\item{start_date}{character string with the time stamp of the first observation} 

\item{stop_date}{character string with the time stamp of the last observation} 

\item{format}{charater string specifying the date-time format of start_date and stop_date (see \code{\link{strptime}} help page)}

\item{spatial_extent}{numeric vector of the form c(long_min,long_max,lan_min,lan_max) that identifies a rectangle used to select the data sources. The lower left corner is (long_min,lan_min) the upper right corner is (long_max,lat_max).}

\item{stationholders}{character vector with the names of the station holders. The list of observation stations is available at \link{http://en.ilmatieteenlaitos.fi/observation-stations}. The available station holders are: 'Weather', 'Precipitation', 'Radar', 'Sounding', 'Mast', 'Buoy', 'Air quality', 'Air quality (urban)'.}

\item{stationholders.exclude}{logical, it is used in combination with \code{stationholders}. If FALSE, then the \code{stationholders} list will be used to select those station holders to include in the output. If TRUE, then the \code{stationholders} list will be used to select those station holders NOT to include in the output. Note that it might happen that a station has more than one station holder, in this case if at least one of the specified \code{stationholders} is included within the station holders of a station then the correspondent action (i.e., include/exclude) will be executed.}

\item{WMOonly}{logical, if TRUE then only WMO stations (i.e., having a WMO code will be returned}

\item{WMOin}{logical, used only if WMOonly=FALSE. if TRUE then WMO stations (i.e., having a WMO code) will be returned otherwise the will not.}

\item{try.again}{numeric value specifying the number of request attemps before giving up}

\item{sleep_sec}{numeric value, number of seconds to wait between two consecutive requests}

\item{na.rm}{logical, if TRUE remove NAs from the output}

\item{url.show}{logical, if TRUE the urls are shown to the user}

\item{file_station_list}{character, full file name of the file with the list of stations}

\item{url4stnlist}{character, link to the page with the list of observation stations (NOT USED, the file_station_list is used instead)}

}

\details{
\code{frost_oldElementCodes} abbreviations are listed at \link{https://frost.met.no/elementtable}. Not all abbreviations have been implemented (see \code{\link{frost_translate_oldElementCodes}}).

As far as I know, FMI API does not return quality codes.

The station metadata is derived from multiple sources.
The observation stations list at \link{"http://en.ilmatieteenlaitos.fi/observation-stations"} is the only place where to get (i) the elevation and (ii) the station group.
On the other hand, the latitude and longitude coordinates available there does have only two decimal digits.
By parsing the xml file retrieved from the FMI API, it is possible to get latitude and longitude with 5 decimal digits and the WMO identifier.
The timestamp is returned as UNIX time

\code{SourceId} is the station identifier of an observation.
The \code{sourceId} is a unique key.

Guidance to the observed values is available at \link{http://en.ilmatieteenlaitos.fi/guidance-to-observations}.
For 'r_1h' hourly precipitation, the timestamp marks the end of the aggregation period.
The other hourly values ('t2m', 'ws_10min', 'p_sea', 'wd_10min', 'wg_10min', 'rh') are actually instantaneous values or 10-minute values referring to the 10-minute interval that ends at minute 00.

For daily (or diurnal) values of: 'rrday' Precipitation is between 0700 UTC and 0700 UTC of the successive day respect to the one reported as the timestamp; 'tmin'  the lowest temperature during two 12 hours period, i.e. between 1900 UTC previous evening and 1900 UTC this evening; 'tmax' the highest temperature during two 12 hours period, i.e. between 1900 UTC previous evening and 1900 UTC this evening; 'tday' the average temperature based usually on 4 or 8 observations per day.

file_station_list is file with a list of station, organized as it follows (column separator must be the tab key):

Name  FMISID  LPNN  WMO Lat Lon Elevation Groups  Started

Alajärvi Möksy  101533  3314  2787  63.09 24.26 171 weather 1957

Alavus Sulkavankylä   101305  2213    62.45 23.55 124 precipitation 1942

...

}

\value{
An object is returned, that is a data frame named \code{fmi_opendata_data}.

Data frame with column names (one row for each \code{timestamp}): \code{timestamp}, \code{sourceId}, \code{lat}, \code{lon}, \code{z}, \code{wmo},  \code{z} (elevation a.m.s.l.), \code{variable1}, \code{variable1_qcode}, \code{variable2}, \code{variable2_qcode}, ...

}

\references{ }

\author{ Cristian Lussana }

\note{ FMI open data licence \link{http://en.ilmatieteenlaitos.fi/open-data-licence}.}

\seealso{ \code{\link{fmi_opendata_assembler}}}

\examples{

# load libraries
library(gibson)
#
auth<-put_your_apiKey_here
#--------------------------------------------------------------------
# get hourly total precipitation data for Norway from 2018-03-04T08:00 UTC to 2018-03-04T13:00 UTC
res<-gibson_fmi_opendata(apiKey=apiKey,
                         frost_oldElementCodes=c("RR_1","UU"),
                         timeResolution="PT1H",
                         start_date="2018-04-10T12",
                         stop_date="2018-04-10T15",
                         format="\%Y-\%m-\%dT\%H")
}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{download, FMI, data.fmi.fi }

