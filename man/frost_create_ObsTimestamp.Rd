\name{frost_create_ObsTimestamp}

\alias{frost_create_ObsTimestamp}

\title{get observation timestamps for Frost data}

\description{
Create observation timestamps for data dowmnloaded using the Frost API \link{https://frost.met.no}.
}

\usage{
frost_create_ObsTimestamp(frost_data=NULL)
}

\arguments{
\item{frost_data}{data frame such as the \code{frost_data} list element returned by \code{\link{gibson_frost}}. The used fields are \code{timeResolution}, \code{timeOffset} and \code{referenceTime}.}
}

\details{
Observations associated to elements with hourly \code{timeResolution} have the timestamp equals to \code{referenceTime}.

Observations associated to elements with daily \code{timeResolution} have the timestamp that is the day derived from \code{referenceTime} plus \code{timeOffset} hours. 
In general, a daily value represets an aggregated value over 24 hours and the timestamp marks the end of the aggregation period. However, if \code{timeOffset} is equal to 00 hours then the timestamp indicates that the observed value represent the aggregation over the day in the timestamp and one should think at the end of the aggregation period as 23:59 UTC (hours:minutes) of the day.
}

\value{
A vector of \code{class=c("POSIXct", "POSIXt")} with the same number of elements of \code{frost_data}.
}

\references{ }

\author{ Cristian Lussana }

\note{ } 

\seealso{ \code{\link{gibson_frost}}  \code{\link{frost_assembler}}}

\examples{

# load libraries
library(gibson)
#
auth<-put_your_ClientID_here
#
frost<-gibson_frost(client_id=auth,oldElementCodes=c("RR_1","TAMRR"), sources="ALL",start_date="2018-03-02T08:00",stop_date="2018-03-04T13:00",format="\%Y-\%m-\%dT\%H:\%M",countries="NO",stationholders="MET.NO",url.show=T)
timestamp<-frost_create_ObsTimestamp(frost$frost_data)

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{MET Norway, frost.met.no, timestamp, observation time }

