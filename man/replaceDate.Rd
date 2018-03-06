\name{replaceDate}
\alias{replaceDate}
\title{replace elements of a string with date and time elements}
\description{
 Replace elements of a string with date and time elements.
}
\usage{
replaceDate(string=NULL, date.str=NULL, format = "\%Y-\%m-\%d \%H:\%M:\%S")

}
\arguments{
  \item{string}{a string with the elements to replace with the date and time elements. For example, if the string "yyyy/yyyymmdd" is provided then yyyy will be replaced with the year, mm with the month and dd with the day. Legend: yyyy year; mm month; dd day; hh hour.}
  \item{date}{a date-time string, such as for example "2017.01.01"}
  \item{format}{the format to be used for the output string. For the
          list of items see the help page of \code{strptime}}
}
\details{
  These functions invoke the function \code{str2Rdate} and the 
  built-in functions \code{formatC}, \code{as.POSIXlt}, \code{gsub} 
  and \code{strptime}.
}
\value{
  str2Rdate: a string of the same lenght as the input string where the elements
  yyyy,mm,dd,hh have been replaced by the date-time elements.
}
\author{ Cristian Lussana }

\seealso{ \code{\link{year}}, \code{\link{month}}, \code{\link{day}} }
\examples{
a <- replaceDate("yyyymm/hello_yyyymmdd.txt","2017.01.01.12","\%Y.\%m.\%d.\%H")

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{ misc }

