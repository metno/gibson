\name{frost_translate_oldElementCodes}

\alias{frost_translate_oldElementCodes}

\title{translate oldElementCodes into Frost API standards}

\description{
Translate \code{oldElementCodes} into Frost API standards  \link{https://frost.met.no}.
Reference is \link{https://frost.met.no/elementtable}.
}

\usage{
frost_translate_oldElementCodes<-function(oldElementCodes=NULL)
}

\arguments{
\item{oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). Available abbreviations are: \\"RR_1\\",\\"TA\\",\\"TAM\\",\\"TAMRR\\",\\"RR\\". See Details for more information or \link{https://frost.met.no/elementtable}.}
}

\details{
\code{oldElementCodes} abbreviation meanings are: \\"RR_1\\" hourly total precipitation, \\"TA\\" two-metre air temperature / hourly sampling rate, \\"TAM\\" daily mean temperature (from 1800 UTC previous day to 1800 UTC observation day), \\"TAMRR\\" daily mean temperature (from 0600 UTC previous day to 0600 UTC observation day), \\"RR\\" daily total precipitation (from 0600 UTC previous day to 0600 UTC observation day). 
}

\value{
A data frame with the same number of rows as the number of elements in \code{oldElementCodes}.
The columns are: \code{elementId}, \code{timeOffset}, \code{timeResolution}, \code{level.value}, \code{level.levelType}
}

\references{ }

\author{ Cristian Lussana }

\note{ } 

\seealso{ \code{\link{gibson_frost}}  \code{\link{frost_assembler}}}

\examples{

# load libraries
library(gibson)
#
out<-frost_translate_oldElementCodes(c("TA","RR_1","TAMRR"))
out

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{MET Norway, frost.met.no }

