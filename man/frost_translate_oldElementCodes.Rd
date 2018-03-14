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
\item{oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). The abbreviations are listed at \link{https://frost.met.no/elementtable}.}
}

\details{
In case an abbreviations is not yet implemented in \code{\link{gibson}} then the \code{elementId} character string is set to \code{""}.
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

