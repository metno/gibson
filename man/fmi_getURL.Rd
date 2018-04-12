\name{fmi_getURL}

\alias{fmi_getURL}

\title{FMI API url to download data}

\description{
Build up the FMI API url that will be used to download data from \code{\link{gibson_fmi_opendata}}.
Linked to the FMI API (\link{data.fmi.fi}).
}

\usage{
fmi_getURL(apiKey=NULL,
           abbrv=NULL, #"minute" "day"
           starttime=NULL,
           endtime=NULL,
           format="%Y-%m-%dT%H:%M",
           parameters=NULL,
           bbox=c(19.09,59.3,31.59,70.13)) 
}

\arguments{

\item{apiKey}{API-key that can be gotten via registration \link{https://ilmatieteenlaitos.fi/rekisteroityminen-avoimen-datan-kayttajaksi}}

\item{abbrv}{character string. It can be either 'minute' or 'day'.}

\item{starttime}{character string with the time stamp of the first observation} 

\item{endtime}{character string with the time stamp of the last observation} 

\item{format}{charater string specifying the date-time format of start_date and stop_date (see \code{\link{strptime}} help page)}

\item{parameters}{charater vector specifying the FMI API parameters. 
Allowed values for hourly variables are: 'r_1h', 't2m', 'ws_10min', 'p_sea', 'wd_10min', 'wg_10min', 'rh'.
Allowed values for daily variables are: 'rrday', 'tday', 'tmin', 'tmax'.}

\item{bbox}{numeric vector of the form c(long_min,long_max,lan_min,lan_max) that identifies a rectangle used to select the data sources. The lower left corner is (long_min,lan_min) the upper right corner is (long_max,lat_max).}

}

\details{
List of stored queries: \link{http://data.fmi.fi/fmi-apikey/insert-your-apikey-here/wfs?request=listStoredQueries} or for more details \link{http://data.fmi.fi/fmi-apikey/insert-your-apikey-here/wfs?request=describeStoredQueries}.
The used queries are (i) abbrv='minute' that is linked to the query 'fmi::observations::weather::multipointcoverage' (ii) abbrv='day' linked to 'fmi::observations::weather::daily::multipointcoverage'

}

\value{
A character string with the url.
}

\references{ }

\author{ Cristian Lussana }

\note{ FMI open data licence \link{http://en.ilmatieteenlaitos.fi/open-data-licence}.}

\seealso{ \code{\link{fmi_opendata_assembler}}}

\examples{

# load libraries
library(gibson)
#
apiKey<-put_your_apiKey_here
}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{url, FMI, data.fmi.fi }

