#+ get in-situ observations from smhi opendata REST API
`smhi_opendata_assembler`<-function(frost_oldElementCodes=NULL,
                                    version="1.0",
                                    station_set_all=T,
                                    latest_hour=T,
                                    latest_day=F,
                                    latest_months=F,
                                    formatOUT="%Y-%m-%dT%H",
                                    coords=data.frame(x="lon",y="lat",
                                                      proj4="+proj=longlat +datum=WGS84",
                                                      stringsAsFactors=F),
                                    na.rm=T,
                                    url.show=F,
                                    verbose=F) {
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
#
  suppressPackageStartupMessages(require(jsonlite))
  suppressPackageStartupMessages(require(sp))
  suppressPackageStartupMessages(require(rgdal))
#
#------------------------------------------------------------------------------
# Checks
  if (is.null(frost_oldElementCodes)) return(NULL)

  if (!(station_set_all & latest_hour) & !latest_months & !latest_day) {
    print("ERROR option not yet implemented")
    return(NULL)
  }

  ElParIds<-smhi_opendata_translate_oldElementCodes(frost_oldElementCodes)
  if (any(is.na(ElParIds))) {
    print("ERROR at least one oldElementCodes is not available in gibson")
    print(ElParIds[which(is.na(ElParIds))])
    return(NULL)
  }
  #
  if (is.null(coords)) {
    print("ERROR in the coord specification")
    return(NULL)
  }
  if (length(coords$x)==0 | length(coords$y)==0 | length(coords$proj4)==0)  {
    print("ERROR in the coord specification")
    return(NULL)
  }
#
#------------------------------------------------------------------------------
  # Retrieve data and metadata from the Frost API
  if (verbose) print("gibson_smhi_opendata")
  res<-gibson_smhi_opendata(frost_oldElementCodes=frost_oldElementCodes,
                            version=version,
                            station_set_all=station_set_all,
                            latest_hour=latest_hour,
                            latest_day=latest_day,
                            latest_months=latest_months,
                            try.again=3,
                            sleep_sec=5,
                            na.rm=na.rm,
                            url.show=url.show)
  if (is.null(res)) {
    print("ERROR while retriving data")
    return(NULL)
  }
  if (length(res$smhi_opendata_meta$station_key)==0) {
    print("WARNING No data")
    return(NULL)
  }
  if (length(res$smhi_opendata_data$elements)==0) {
    print("WARNING No data")
    return(NULL)
  }
  count <- 0
  for (i in 1:length(res$smhi_opendata_data$elements))
    count <- count + length(res$smhi_opendata_data$elements[[i]]$date_time)
  if (count == 0) {
    print("WARNING No data")
    return(NULL)
  }
#
#------------------------------------------------------------------------------
# spatial coordinates
  if (verbose) print("spatial coordinates")
  ncrs<-length(coords$x)
  ncoords<-2*length(coords$x)
  nval<-length(res$smhi_opendata_meta$lon)
  if (nval>0) {
    coords.val<-as.data.frame(array(data=NA,dim=c(nval,ncoords)))
    # smhi_opendata eturns coordinates as long-lat
    xy.from<-data.frame(x=res$smhi_opendata_meta$lon,
                        y=res$smhi_opendata_meta$lat)
    proj4.from<-"+proj=longlat +datum=WGS84"
    for (i in 1:length(coords$x)) {
      ctmp<-coordinates(crs_transform(xy.from=xy.from,
                                      proj4.from=proj4.from,
                                      proj4.to=coords$proj4[i]))
      coords.val[,((i-1)*2+1):(i*2)]<-ctmp
      if (i==1) {
        costr<-c(coords$x[1],coords$y[1])
      } else {
        costr<-c(costr,coords$x[i],coords$y[i])
      }
    }
    names(coords.val)<-costr
  } else {
    print("no metadata from smhi_opendata")
  }
  res$smhi_opendata_meta$coords <- coords.val
#
#------------------------------------------------------------------------------
# assembly output
  if (verbose) print("assembly  output")
  noldElementCodes<-length(ElParIds)
  for (v in 1:noldElementCodes) {
    res$smhi_opendata_data$elements[[v]]$date_time_stamp <- 
      Rdate2str( as.POSIXct( res$smhi_opendata_data$elements[[v]]$date_time / 1000,
                             origin="1970-01-01", tz="UTC"),
                             format=formatOUT)
  }
#------------------------------------------------------------------------------
    return(res)
}
