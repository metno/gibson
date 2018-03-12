\name{frost_removeDuplicates}

\alias{frost_removeDuplicates}

\title{remove duplicate records from Frost API data}

\description{
Remove duplicates records from the output of \code{gibson_frost}, that is the KDVH data retrieved through the Frost API \link{frost.met.no}.

Duplicates are defined as one or more records refererring to the same sourceId(station:sensor), observation_timestamp and having the same observed value and quality flag value.
This situation might occur because the same data are retrieved by Frost from different KDVH tables.
}

\usage{
frost_removeDuplicates(oldElementCode=NULL,
                       frost_data=NULL) 
}

\arguments{
\item{oldElementCode}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). Available abbreviations are: \\"RR_1\\",\\"TA\\",\\"TAM\\",\\"TAMRR\\",\\"RR\\". See Details for more information or \link{https://frost.met.no/elementtable}.}

\item{frost_data}{the data frame frost_data returned by \code{gibson_frost}}
}

\details{

Observations with time resolution smaller than 1 day does not have duplicates.

Diurnal data might have duplicates.
}

\value{
A numeric vector with the position of the NON-duplicates in \code{frost_data}.
}

\references{ }

\author{ Cristian Lussana }

\note{ } 

\seealso{\code{\link{gibson_frost}} \code{\link{first_assembler}}}

\examples{

# load libraries
library(gibson)
# get the data
frost<-gibson_frost(client_id=auth,oldElementCodes=c("RR"), sources="ALL",start_date="2018-03-02T08:00",stop_date="2018-03-04T13:00",format="\%Y-\%m-\%dT\%H:\%M",countries="NO",stationholders="MET.NO",url.show=T)

res<-frost_removeDuplicates(oldElementCode="RR",frost_data=frost$frost_data)

#> length(res)
#[1] 687
#> length(frost$frost_data$sourceId)
#[1] 1297

# create a data frame without duplicates
frost_data_noDuplicates<-data.frame(frost$frost_data$elementId[ixNoDup],frost$frost_data$sourceId[ixNoDup],frost$frost_data$referenceTime[ixNoDup],frost$frost_data$value[ixNoDup],frost$frost_data$qcode[ixNoDup],frost$frost_data$timeOffset[ixNoDup],frost$frost_data$timeResolution[ixNoDup],frost$frost_data$level[ixNoDup],frost$frost_data$levelType[ixNoDup],frost$frost_data$oldElementCodes[ixNoDup],stringsAsFactors=F)
names(frost_data_noDuplicates)<-c("elementId","sourceId","referenceTime","value","qcode","timeOffset","timeResolution","level","levelType","oldElementCodes")

# write files
write.table(frost$frost_data,file="data_original")
write.table(frost_data_noDuplicates,file="data_noDuplicates")
}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{coordinate reference system, transformation }

