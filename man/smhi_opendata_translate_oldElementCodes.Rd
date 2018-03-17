\name{smhi_opendata_translate_oldElementCodes}

\alias{smhi_opendata_translate_oldElementCodes}

\title{translate oldElementCodes into smhi_opendata API standards}

\description{
Translate \code{frost_oldElementCodes} into smhi_opendata API standards  \link{https://opendata-download-metobs.smhi.se/}.
Reference for frost_oldElementCodes is \link{https://frost.met.no/elementtable}.
}

\usage{
smhi_opendata_translate_oldElementCodes<-function(oldElementCodes=NULL)
}

\arguments{
\item{oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). The abbreviations are listed at \link{https://frost.met.no/elementtable}.}
}

\details{
In case an abbreviations is not yet implemented in \code{\link{gibson}} then the \code{parameterId} character string is set to \code{NA}.

The SMHI API \code{parameterId} specifies the meteorological or climate element and the post-processing action by means of a numerical code (see \link{https://opendata-download-metobs.smhi.se/api/version/1.0.json}).
}

\value{
A numeric vector with the same number of rows as the number of elements in \code{oldElementCodes}.
}

\references{ }

\author{ Cristian Lussana }

\note{ } 

\seealso{ \code{\link{gibson_frost}}  \code{\link{frost_assembler}}}

\examples{

# load libraries
library(gibson)
#
out<-smhi_opendata_translate_oldElementCodes(c("TA","RR_1","TAMRR"))
out

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{MET Norway, SMHI }

