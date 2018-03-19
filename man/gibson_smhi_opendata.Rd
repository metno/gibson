\name{gibson_smhi_opendata}

\alias{gibson_smhi_opendata}

\title{get SMHI in-situ data}

\description{
Download metadata and observations from the database of in-situ observation of the Swedish Meterological and Hydrological Institute (SMHI) by using the SMHI Open Data Meteorological Observations REST API (\link{https://opendata-download-metobs.smhi.se}).
In particular, this function has been implemented and tested to download hourly data.

The database includes in-situ data measured by the network of sensors located at stations belonging to several different station holders (not only SMHI).
One station has a unique owner.
It is possible to retrieve both the measurements and the post-processing of the measured values, together with the correspondent quality flags when available.
The post-pocessing usually aggregates the measured values in different ways (mean/max/min/...) over different time intervals (hours/days/months/years/...). 

A single observed value returned by \code{gibson_smhi_opendata} can be univocally identified by the union of several pieces of information, such as (for our purposes): \code{station_key}, \code{parameterId}, \code{date}.
The \code{station_key} identifies the station.
The \code{parameterId} specifies the meteorological or climate element and the post-processing action by means of a numerical code (see \link{https://opendata-download-metobs.smhi.se/api/version/1.0.json}). 
The \code{date} specifies the observation time or timestamp (time standard is Coordinated Universal Time or UTC).
For hourly data, the timestamp specifies the end of the aggregation period, se for example the page \link{https://opendata-download-metobs.smhi.se/api/version/latest/parameter/1/station-set/all/period/latest-hour.json} where the fields \code{from} and \code{to} give the extreme of the time aggregation period.
The correspondent downloaded data \link{https://opendata-download-metobs.smhi.se/api/version/latest/parameter/1/station-set/all/period/latest-hour/data.json} report the end of the aggregation period as the \code{date}.

For convenience, it is possible to request the Weather and Climate Elements also by using their MET Norway's old element codes \code{\link{gibson_frost}}.

Have a look to \code{\link{smhi_opendata_assembler}} to find examples on how to post-process \code{gibson_smhi_opendata} results in order to obtain the desired output.
}

\usage{
gibson_smhi_opendata(frost_oldElementCodes=NULL,
                     version="1.0",
                     station_set_all=T,
                     latest_hour=T,
                     try.again=1,
                     sleep_sec=5,
                     na.rm=T,
                     url.show=F)
}

\arguments{

\item{frost_oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). See Details for more information or \link{https://frost.met.no/elementtable}.}

\item{version}{character string with the API version \link{https://opendata.smhi.se/apidocs/metobs/version.html}}

\item{station_set_all}{logical. If TRUE then the station set is set to "all" \link{https://opendata.smhi.se/apidocs/metobs/stationSet.html}. Note that according to the SMHI documentation station-set set to all is only possible in combination with "latest hour" as period.}

\item{latest_hour}{logical. If TRUE then the hourly data of the latest hour are downloaded. In fact, there is a predefined query to do that, see the JSON example at \link{https://opendata.smhi.se/apidocs/metobs/data.html}.}

\item{try.again}{numeric value specifying the number of request attemps before giving up}

\item{sleep_sec}{numeric value, number of seconds to wait between two consecutive requests}

\item{na.rm}{logical, if TRUE remove NAs from the output}

\item{url.show}{logical, if TRUE the urls are shown to the user}

}

\details{

API Response Messages (HTTP_Status_Code Reason) are reported at \link{https://opendata.smhi.se/apidocs/metobs/common.html#httpcodes}.

The date is returned as it is downloaded from the API, which is UNIX time in milliseconds from 1970-01-01.
For hourly data, the timestamp specifies the end of the aggregation period.
}

\value{
A list with two objects is returned. The objects are: \code{smhi_opendata_data}, \code{smhi_opendata_meta}.

\code{smhi_opendata_data}. Data frame with column names: \code{parameterId}, \code{station_key}, \code{date}, \code{value} (observed value), \code{qcode} (quality flag), \code{frost_oldElementCodes}.

The quality code  can be ''either G (green), Y (yellow) or R (red) depending on the quality of the value'', see \link{https://opendata.smhi.se/apidocs/metobs/schemas.html#specific} under metObsSampleValueType and metObsIntervalValueType.

\code{smhi_opendata_meta}. Data frame with column names: \code{station_key}, \code{lon} (longitude), \code{lat} (latitude), \code{z} (elevation m.a.m.s.l.), \code{owner} (station owner).

\code{smhi_open_data} has the dimension of the number of observations returned and the link between this structure and the others two is the \code{station_key}.

}

\references{ }

\author{ Cristian Lussana }

\note{ data licence \link{https://opendata.smhi.se/apidocs/metobs/index.html#license}}

\seealso{ \code{\link{gibson_frost}} \code{\link{smhi_opendata_assembler}}}

\examples{

# load libraries
library(gibson)
library(jsonlite)
#

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{download, SMHI, Sweden, opendata.smhi.se }

