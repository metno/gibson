\name{gibson_frost}

\alias{gibson_frost}

\title{get MET Norway in-situ data}

\description{
Download metadata and observations from the Climate Database (KDVH) of the Norwegian Meterological Institute (MET Norway or MET.NO) by using the Frost API (frost.met.no).
In particular, this function has been implemented and tested to download hourly and diurnal data, though it can be used for other aggregation intervals too.
The Frost documentation is available at frost.met.no.

The KDVH contains in-situ data measured by the network of sensors located at stations belonging to several different station holders (not only "MET.NO").
One station can belong to more than one station holder.
It is possible to retrieve both the measurements and the post-processing of the measured values, together with the correspondent quality flags when available.
The post-pocessing usually aggregates the measured values in different ways (mean/max/min/...) over different time intervals (hours/days/months/years/...). 

A single observed value returned by \code{gibson_frost} can be univocally identified by the union of several pieces of information, such as (for our purposes): \code{sourceId}, \code{elementId}, \code{referenceTime}, \code{timeOffset}, \code{timeResolution}, \code{level.value}, \code{level.levelType}.
The \code{sourceId} identifies the station/sensor pair. The \code{elementId} specifies the meteorological or climate element (variable/aggregation pair). The triplet \code{referenceTime}, \code{timeOffset}, \code{timeResolution} defines the observation timestamp (time standard is Coordinated Universal Time or UTC) as designed by the Frost API developers. The pair \code{level.value}, \code{level.levelType} specifies at which level the observation has been taken (two-metre above the surface, 10-metre above the surface,...).
For more information about the meaning of the different fields, refers also to the description of the \code{gibson_frost} arguments.

For convenience, it is possible to request the Weather and Climate Elements also by using their old element codes (in use before Frost).

Have a look to \code{\link{frost_assembler}} to find examples on how to post-process \code{gibson_frost} results in order to obtain the desired output.
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
             format="\%Y-\%m-\%dT\%H:\%M",
             countries="NO",
             spatial_extent=c(4,34,54,72),
             stationholders=NULL,
             stationholders.exclude=F,
             doit.meta=T,
             doit.data=T,
             WMOonly=F,
             try.again=1,
             sleep_sec=5,
             na.rm=T,
             url.show=F)
}

\arguments{

\item{client_id}{character string with the API Client ID \link{https://frost.met.no/auth/requestCredentials.html}}

\item{oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). See Details for more information or \link{https://frost.met.no/elementtable}.}

\item{elementId}{character vector with Weather and Climate Elements Id \link{https://frost.met.no/elementtable}}

\item{timeOffset}{character vector with the time offsets (list of ISO-8601 periods, e.g. 'PT06H,PT18H'). \link{https://frost.met.no/reference#!/observations/timeSeries}}

\item{timeResolution}{character vector with the time resolutions (list of ISO-8601 periods, e.g. 'PT06H,PT18H'). \link{https://frost.met.no/reference#!/observations/timeSeries}}

\item{level.value}{numeric vector with the sensor levels. \link{https://frost.met.no/reference#!/observations/timeSeries}}

\item{level.levelType}{character vector with the sensor level types (e.g., "height_above_ground"). \link{https://frost.met.no/reference#!/observations/timeSeries}}

\item{sources}{character vector with the source Ids, use "ALL" to retrieve all the sources within the specified selection. Note that if doit.meta=FALSE then sources="ALL" will generate an error message.}

\item{start_date}{character string with the time stamp of the first observation} 

\item{stop_date}{character string with the time stamp of the last observation} 

\item{format}{charater string specifying the date-time format of start_date and stop_date (see \code{\link{strptime}} help page)}

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

\item{url.show}{logical, if TRUE the urls are shown to the user}

}

\details{
\code{oldElementCodes} abbreviations are listed at \link{https://frost.met.no/elementtable}. Not all abbreviations have been implemented (see \code{\link{frost_translate_oldElementCodes}}).

\code{SourceId} is the data source identifier of an observation. From the documentation on frost.met.no:  \\"The ID(s) of the data sources to get time series for as a comma-separated  list of Frost API station IDs: SN<int>[:<int>|all]  (e.g. SN18700, SN18700:0, or SN18700:all). 0 is the main sensor and x≥1 is a  parallel sensor.\\"
Remember that the \code{sourceId} is not a unique key.

Frost API Response Messages (HTTP_Status_Code Reason)meaning: (400) Invalid parameter value or malformed request; (401) Unauthorized client ID; (404) No data was found for the list of query Ids; (500) Internal server error.

This function returns the  \code{timeOffset}, \code{timeResolution}, \code{referenceTime} of an observation.
The \code{timeResolution} has the format "\%Y-\%m-\%dT\%H:\%M:\%S".
For the hourly values, the \code{referenceTime} is also the observation timestamp and it mark the end of the aggregation time period.
For diurnal data, the observation timestamp depends on the \code{elementId} and it is the result of an elaboration based on the three fields as described in the documentation of \code{\link{frost_assembler}}.
}

\value{
A list with three objects is returned. The objects are: \code{frost_data}, \code{frost_meta}, \code{stationholders}.

\code{frost_data}. Data frame with column names: \code{elementId}, \code{sourceId}, \code{referenceTime}, \code{value} (observed value), \code{qcode} (quality flag), \code{timeOffset}, \code{timeResolution}, \code{level}, \code{levelType}, \code{oldElementCodes} (different from NAs only if specified as input).

The quality code meanings are available at this link \link{https://frost.met.no/observations/availableQualityCodes/v0.jsonld?lang=nb-NO}.

\code{frost_meta}. Data frame with column names: \code{source}, \code{sensId}, \code{sourceId} (i.e., \code{source}:\code{sensId}), \code{performanceCategory}, \code{exposureCategory}, \code{lon} (longitude), \code{lat} (latitude), \code{z} (elevation m.a.m.s.l.).

\code{stationholders}. List with the station holders for each sensor. Note that a station can belong to more than one station holder.

\code{frost_meta} and \code{stationholders} have the dimension of the number of sensors returned and they follow the same order such that: \code{stationholders[[1]]} refers to \code{frost_meta[2]}; \code{stationholders[[2]]} refers to \code{frost_meta[2]}; ...

\code{frost_data} has the dimension of the number of observations returned and the link between this structure and the others two is the \code{sourceId}.

}

\references{ }

\author{ Cristian Lussana }

\note{ MET Norway Data and products are licensed under Norwegian license for public data (NLOD) and Creative Commons 4.0 BY Internasjonal \link{https://www.met.no/en/free-meteorological-data}.}

\seealso{ \code{\link{frost_assembler}}}

\examples{

# load libraries
library(gibson)
library(jsonlite)
#
auth<-put_your_ClientID_here
#--------------------------------------------------------------------
# get hourly total precipitation data for Norway from 2018-03-04T08:00 UTC to 2018-03-04T13:00 UTC
frost<-gibson_frost(client_id=auth, oldElementCodes="RR_1", sources="ALL", start_date="2018-03-04T08:00", stop_date="2018-03-04T13:00", countries="NO")

# look at the station holders
frost$stationholders[[1]]

names(frost$frost_meta)

# look at the metadata
frost$frost_meta[1:5,]

# look at the data
frost$frost_data[1:5,]

#--------------------------------------------------------------------
# get hourly total precipitation data for Norway from 2018-03-04T08:00 UTC to 2018-03-04T13:00 UTC only for "MET.NO" stations
frost<-gibson_frost(client_id=auth,oldElementCodes="RR_1", sources="ALL",start_date="2018-03-04T08:00",stop_date="2018-03-04T13:00",format="\%Y-\%m-\%dT\%H:\%M",formatOUT="\%Y-\%m-\%dT\%H",countries="NO",stationholders="MET.NO")

#--------------------------------------------------------------------
# get hourly total precipitation data for Norway from 2018-03-04T08:00 UTC to 2018-03-04T13:00 UTC only for NOT "MET.NO" stations
frost<-gibson_frost(client_id=auth,oldElementCodes="RR_1", sources="ALL",start_date="2018-03-04T08:00",stop_date="2018-03-04T13:00",format="\%Y-\%m-\%dT\%H:\%M",formatOUT="\%Y-\%m-\%dT\%H",countries="NO",stationholders="MET.NO",stationholders.exclude=T)

#--------------------------------------------------------------------
# get hourly air_temperature data for Norway from 2018-03-04T08:00 UTC to 2018-03-04T13:00 UTC
frost<-gibson_frost(client_id=auth,oldElementCodes="TA", sources="ALL",start_date="2018-03-04T08:00",stop_date="2018-03-04T13:00",format="\%Y-\%m-\%dT\%H:\%M",formatOUT="\%Y-\%m-\%dT\%H",countries="NO")


#--------------------------------------------------------------------
# get daily total precipitation data for Norway from 2018-01-04T08:00 UTC to 2018-03-04T13:00 UTC
frost<-gibson_frost(client_id=auth,oldElementCodes="RR", sources="ALL",start_date="2018-01-04T08:00",stop_date="2018-03-04T13:00",format="\%Y-\%m-\%dT\%H:\%M",formatOUT="\%Y-\%m-\%dT\%H",countries="NO")
# same as before but only for a smaller domain in southern Norway
frost<-gibson_frost(client_id=auth,oldElementCodes="RR", sources="ALL",start_date="2018-03-01T08:00",stop_date="2018-03-04T13:00",format="\%Y-\%m-\%dT\%H:\%M",formatOUT="\%Y-\%m-\%dT\%H",countries="NO",spatial_extent=c(5,10,58,62))

#--------------------------------------------------------------------
# hourly total precipitation and daily mean temperature (06-06UTC) only for MET.NO stations
frost<-gibson_frost(client_id=auth,oldElementCodes=c("RR_1","TAMRR"), sources="ALL",start_date="2018-03-02T08:00",stop_date="2018-03-04T13:00",format="\%Y-\%m-\%dT\%H:\%M",countries="NO",stationholders="MET.NO",url.show=T)

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{download, MET Norway, frost.met.no }

