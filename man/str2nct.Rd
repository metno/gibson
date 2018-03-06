\name{str2nct}
\alias{str2nct}
\alias{nct2str}
\title{ Convert between date character strings and netcdf time format }
\description{
  Time indicators in netcdf use numbers of elapsed time units since a 
  reference time. (Convertion used by CF standard.) \code{str2nct} evaluates
  the elapsed time of a date with respect to a reference time and generates
  a netcdf representation of the input character string(s). Currently
  available time units are number of minutes, hours, days, months, or years. 
  \code{nct2str} makes the reverse conversion.
}
\usage{
str2nct(str, t.unit, format = "\%Y\%m\%d\%H\%M", 
        t.ref = "190001010000", format.ref = "\%Y\%m\%d\%H\%M")
nct2str(nct, nct.unit, format="\%Y\%m\%d\%H\%M") 
}
\arguments{
  \item{str}{ A character string (or a vector of strings) specifying the
        date. }
  \item{t.unit}{ The desired time unit. Currently available are:
        \code{"T"} delivering minutes, \code{"H"} delivering hours, 
        \code{"D"} delivering days, \code{"M"} delivering months,
        \code{"Y"} delivering years.}
  \item{format}{ The format of the input/output date string. Here we use conventions
        as in \code{format}. Note that all dates need have the same format
        if the input for \code{str} is a vector. }
  \item{t.ref}{ The time reference to use. A character string.}
  \item{format.ref}{ The format of the time reference. }
  \item{nct}{ A netcdf time specification, typically a vector of integers,
        indicating the number of seconds, days etc. elabsed since a reference
        time. }
  \item{nct.unit}{ The netcdf time unit used in the specification of argument
        \code{nct}. A string of the form "days since 1900-01-01 00:00:00"}
}
\details{
  \code{str2nct} always returns integer numbers. So for example if \code{t.unit="M"} 
  \code{str} ist truncated to the first 6 digits and the elabsed number of 
  months is determined for the month of the date.

  Note that the elapsed time starts with 0 if \code{str} is the same 
  like \code{t.ref}.
}
\value{
  A single value or a vector with the elapsed time(s). The output has
  an attribute with name \code{since.lab}. This is the character string
  that can be used as the units attribute for time in the netcdf file.
}
\references{  }
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{Rdate2str}} }
\examples{
ts <- c("200706010000","200606020000","200606030000")
ts.ref="200001010000"
hh <- str2nct(str=ts,t.unit="D",t.ref=ts.ref)
hh

nct2str(nct=hh,nct.unit=attr(hh,"since.lab"))

}
\keyword{ misc }% at least one, from doc/KEYWORDS
