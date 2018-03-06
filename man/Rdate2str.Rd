\name{Rdate2str}
\alias{Rdate2str}
\alias{str2Rdate}
\title{Conversion between date-time strings and R date-time objects}
\description{
  str2Rdate: Convert a string representation of a date-time into 
             an R date-time object of type (POSIXt, POSIXct). 

  Rdate2str: Convert an R date-time object into a string of a 
             desired string format.  
}
\usage{
Rdate2str(date, format = "\%Y-\%m-\%d \%H:\%M:\%S")

str2Rdate(ts, format = "\%Y-\%m-\%d \%H:\%M:\%S")

}
\arguments{
  \item{date}{a date-time object of type POSIXt, POSIXct}
  \item{ts}{a date-time string having the format of \code{format}}
  \item{format}{the format to be used for the output string. For the
          list of items see the help page of \code{strptime}}
}
\details{
  These functions invoke the built-in functions \code{format}, 
  \code{as.POSIXct} and \code{strptime}.

}
\value{
  str2Rdate: a date-time object of R (POSIXt, POSIXct) which is essentially
  the number of seconds since some fixed time.
  Rdate2str: a string with a sequence of the sort year - month - day - time
  following the format specification in argument format.
}
\author{ Christoph Frei }

\seealso{ \code{\link{year}}, \code{\link{month}}, \code{\link{day}} }
\examples{
hello.e <- str2Rdate("1977.09.28", format="\%Y.\%m.\%d")
Rdate2str(hello.e-5597*86400,format="\%Y-\%m-\%d")

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{ misc }

