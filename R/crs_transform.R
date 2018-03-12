# + transformation of coordinates between coordinate reference systems
`crs_transform`<-function(xy.from=NULL,
                          proj4.from=NULL,
                          proj4.to=NULL)
{
#------------------------------------------------------------------------------
# xy.from. data frame with two numeric columns "x" and "y" (i.e., the Easting and Northing coordinates)
# proj4.from. coordinate reference system of xy.from as a proj4 string
# proj4.to. target coordinate reference system as a proj4 string
# returned value
#  data frame of two numeric columns with the  transformed coordinates
#------------------------------------------------------------------------------
#  This file is free software: you may copy, redistribute and/or modify it  
#  under the terms of the GNU General Public License as published by the  
#  Free Software Foundation, either version 2 of the License, or (at your  
#  option) any later version.  
#  
#  This file is distributed in the hope that it will be useful, but  
#  WITHOUT ANY WARRANTY; without even the implied warranty of  
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
#  General Public License for more details.  
#  
#  You should have received a copy of the GNU General Public License  
#  along with this program.  If not, see <http://www.gnu.org/licenses/>. 
#------------------------------------------------------------------------------
  require(rgdal)
  require(sp)
  if (is.null(xy.from) | is.null(proj4.from) | is.null(proj4.to))
    return(NULL)
  xy.to<-spTransform(SpatialPoints(cbind(xy.from$x,xy.from$y),
                                       proj4string=CRS(proj4.from)),
                     CRS(proj4.to))
  xy.to 
}
