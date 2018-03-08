\name{gibson_frost}
\alias{gibson_frost}
\title{get data from frost.met.no}
\description{
Retrieve metadata and observations from frost.met.no.
The Frost API retrieve observations and metadata from the Climate Database (KDVH) of the Norwegian Meterological Institute (MET Norway or MET.NO).
Frost documentation is available at frost.met.no.

The KDVH contains in-situ observations measured by sensors located at stations belonging to several different station holders (not only "MET.NO").
One station can belong to more than one station holder.

The key used by frost is the data source identifier of an observation (sourceId). From the documentation on frost.met.no:  \\"The ID(s) of the data sources to get time series for as a comma-separated  list of Frost API station IDs: SN<int>[:<int>|all]  (e.g. SN18700, SN18700:0, or SN18700:all). 0 is the main sensor and x≥1 is a  parallel sensor.\\"
The sourceId is not a unique key.
In fact, for the same data source the observations stored in the KDVH are the raw measurements and the post-processed values.
The post-pocessing usually aggregates the measured values in different ways over different time intervals (hours/days/months/years/...). 
The time stamps associated to the observed values mark the end of the aggregation period. 
The time zone assumed for the time stamps is UTC.
The unique identifier of an observed value is the union of several piece of information. 
For our purposes the following fields are used to build up the unique identifier: sourceId, elementId, timeOffset, timeResolution, level.value, level.levelType.
The meaning of the different fields is described in the section on the function arguments.

For convenience, it is possible to specify the Weather and Climate Elements needed also by using the old element codes (used before Frost).
The available old codes are: \\"RR_1\\",\\"TA\\",\\"TAM\\",\\"TAMRR\\",\\"RR\\". 

Some of the observations, either raw measurements or post-processed, collected in the KDVH are quality checked and the information is available through Frost.

}
\usage{
gibson_frost(client_id=NULL,
             oldElementCodes=NULL,
             elementId=NULL,
             timeOffset=NULL,
             timeResolution=NULL,
             level.value=NULL,
             level.levelType=NULL,
             sources="ALL",
             start_date=NULL,
             stop_date=NULL,
             format="%Y-%m-%dT%H:%M",
             formatOUT="%Y-%m-%dT%H",
             countries="NO",
             spatial_extent=c(4,34,54,72),
             stationholders=NULL,
             stationholders.exclude=F,
             doit.meta=T,
             doit.data=T,
             WMOonly=F,
             try.again=1,
             sleep_sec=5,
             na.rm=T)
}
\arguments{

\item{client_id}{character string with the API Client ID \link{https://frost.met.no/auth/requestCredentials.html}}

\item{oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). Available abbreviations are: \\"RR_1\\",\\"TA\\",\\"TAM\\",\\"TAMRR\\",\\"RR\\". See Details for more information or \link{https://frost.met.no/elementtable}.}

\item{elementId}{character vector with Weather and Climate Elements Id \link{https://frost.met.no/elementtable}}

\item{timeOffset}{character vector with the time offsets (list of ISO-8601 periods, e.g. 'PT06H,PT18H'). \link{https://frost.met.no/reference#!/observations/timeSeries}}

\item{timeResolution}{character vector with the time resolutions (list of ISO-8601 periods, e.g. 'PT06H,PT18H'). \link{https://frost.met.no/reference#!/observations/timeSeries}}

\item{level.value}{numeric vector with the sensor levels. \link{https://frost.met.no/reference#!/observations/timeSeries}}

\item{level.levelType}{character vector with the sensor level types (e.g., "height_above_ground"). \link{https://frost.met.no/reference#!/observations/timeSeries}}

\item{sources}{character vector with the source Ids, use "ALL" to retrieve all the sources within the specified selection. Note that if doit.meta=FALSE then sources="ALL" will generate an error message.}

\item{start_date}{character string with the time stamp of the first observation} 

\item{stop_date}{character string with the time stamp of the last observation} 

\item{format}{charater string specifying the date-time format of start_date and stop_date (see \code{\link{strptime}} help page)}

\item{formatOUT}{charater string specifying the date-time format desired for the the output (see strptime help page)}

\item{countries}{character vector with the abbreviations of the countries. Only data sources located in those countries will be returned.}

\item{spatial_extent}{numeric vector of the form c(long_min,long_max,lan_min,lan_max) that identifies a rectangle used to select the data sources. The lower left corner is (long_min,lan_min) the upper right corner is (long_max,lat_max).}

\item{stationholders}{character vector with the names of the station holders}

\item{stationholders.exclude}{logical, it is used in combination with stationholders If FALSE, then the stationholder list will be used to select the station holders to include in the output.  If TRUE, then the stationholder list will be used to select the station holders NOT to include in the output.}

\item{doit.meta}{logical. If set to TRUE then the source metadata are retrieved}

\item{doit.data}{logical. If set to TRUE then the observed values are retrieved} 

\item{WMOonly}{logical, if TRUE then only WMO stations (i.e., having a WMO code will be returned}

\item{try.again}{numeric value specifying the number of request attemps before giving up}

\item{sleep_sec}{numeric value, number of seconds to wait between two consecutive requests}

\item{na.rm}{logical, if TRUE remove NAs from the output}

}
\details{
  \code{oldElementCodes} abbreviation meanings are: \\"RR_1\\" hourly total precipitation, \\"TA\\" two-metre air temperature / hourly sampling rate, \\"TAM\\" daily mean temperature (from 1800 UTC previous day to 1800 UTC observation day), \\"TAMRR\\" daily mean temperature (from 0600 UTC previous day to 0600 UTC observation day), \\"RR\\" daily total precipitation (from 0600 UTC previous day to 0600 UTC observation day). 

}

\value{
  TODO 
}
\author{ Cristian Lussana }

\seealso{ \code{\link{replaceDate}} }
\examples{
TODO

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{ observation, frost.met.no }

