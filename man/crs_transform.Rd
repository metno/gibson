\name{crs_transform}

\alias{crs_transform}

\title{transformation of coordinates between coordinate reference systems}

\description{
Transformation of coordinates between coordinate reference systems (CRSs).
The CRS are specified with proj4 strings \link{http://proj4.org/}.
A list of proj4 string can be found at \link{http://spatialreference.org/}.
}

\usage{
crs_transform(xy.from=NULL,
              proj4.from=NULL,
              proj4.to=NULL)
}

\arguments{
\item{xy.from}{data frame with two columns: x and y. They are the Easting and Northing coordinates to be transformed.}

\item{proj4.from}{proj4 string specifying the CRS for xy.from.}

\item{proj4.to}{proj4 string specifyng the CRS for the transformed coordinates.}

}

\details{}
\value{
An object of class SpatialPoints fromt the R-package \code{sp}.
}

\references{ }

\author{ Cristian Lussana }

\note{This function requires the R-packages \code{rgdal} and \code{sp} } 

\seealso{}

\examples{

# load libraries
library(gibson)
# transformation from long-lat to UTM33 CRSs
res<-crs_transform(data.frame(x=c(45,50),y=c(12,0)),proj4.from="+proj=longlat +datum=WGS84",proj4.to="+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# from SpatialPoins object to a matrix
mat<-coordinates(res)
x.to<-mat[,1]
y.to<-mat[,2]
}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{coordinate reference system, transformation }

