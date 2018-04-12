\name{fmi_opendata_translate_oldElementCodes}

\alias{fmi_opendata_translate_oldElementCodes}

\title{translate MET Norway oldElementCodes into FMI API standards}

\description{
Translate \code{oldElementCodes} into FMI API standards  \link{https://data.fmi.fi} and viceversa.
\code{oldElementCodes} reference is \link{https://frost.met.no/elementtable}.
}

\usage{
fmi_opendata_translate_oldElementCodes<-function(oldElementCodes=NULL,
                                                 fmi_parameterId=NULL)
}

\arguments{
\item{oldElementCodes}{character vector with the abbreviations used to define weather and climate elements (before Frost-era). The abbreviations are listed at \link{https://frost.met.no/elementtable}.}

\item{fmi_parameterId}{charater vector specifying the FMI API parameters. 
Allowed values for hourly variables are: 'r_1h', 't2m', 'ws_10min', 'p_sea', 'wd_10min', 'wg_10min', 'rh'.
Allowed values for daily variables are: 'rrday', 'tday', 'tmin', 'tmax'.}
}

\details{
In case an abbreviations is not yet implemented in \code{\link{gibson}} then the \code{elementId} character string is set to \code{""}.

\code{oldElementCodes} and \code{fmi_parameterId} cannot be both not null objects.
If \code{oldElementCodes} is not null then translate the MET Norway element code into the FMI API.
If \code{fmi_parameterId} is not null then translate the FMI API element name into the MET Norway element code.
 
}

\value{
A character vector with the same number of elements as the number of elements in the input vector.
}

\references{ }

\author{ Cristian Lussana }

\note{ } 

\seealso{ \code{\link{gibson_frost}}  \code{\link{frost_assembler}}}

\examples{

# load libraries
library(gibson)
#
out<-fmi_opendata_translate_oldElementCodes(c("TA","RR_1","TAMRR"))
out

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{MET Norway, frost.met.no }

