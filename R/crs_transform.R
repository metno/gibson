# + transformation of coordinates between coordinate reference systems
`crs_transform`<-function(xy.from=NULL,
                          proj4.from=NULL,
                          proj4.to=NULL)
{
#
# xy.from. data frame with two numeric columns "x" and "y" (i.e., the Easting and Northing coordinates)
# proj4.from. coordinate reference system of xy.from as a proj4 string
# proj4.to. target coordinate reference system as a proj4 string
# returned value
#  data frame of two numeric columns with the  transformed coordinates
#---------------------------------------------------------------------------
  require(rgdal)
  require(sp)
  if (is.null(xy.from) | is.null(proj4.from) | is.null(proj4.to))
    return(NULL)
  xy.to<-spTransform(SpatialPoints(cbind(xy.from$x,xy.from$y),
                                       proj4string=CRS(proj4.from)),
                     CRS(argv$proj4to))
  xy.to 
}
