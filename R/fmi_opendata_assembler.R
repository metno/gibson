#+ get in-situ observations from http://data.fmi.fi
`fmi_opendata_assembler`<-function(apiKey=NULL,
                                   oldElementCodes=NULL,
                                   timeResolution=NULL, # PT1H or P1D
                                   start_date=NULL,
                                   stop_date=NULL,
                                   format="%Y-%m-%dT%H:%M",
                                   formatOUT="%Y-%m-%dT%H",
                                   spatial_extent=c(19.09,59.3,31.59,70.13),
                                   stationholders=NULL,
                                   stationholders.exclude=F,
                                   WMOonly=F,
                                   WMOin=T,
                                   na.rm=T,
                                   url.show=F,
                                   coords=data.frame(x="lon",y="lat",
                                          proj4="+proj=longlat +datum=WGS84",
                                          stringsAsFactors=F),
          url4stnlist="http://en.ilmatieteenlaitos.fi/observation-stations",
                                   verbose=F)
{
#------------------------------------------------------------------------------
# Documentation:see help(frost_assembler) on R or gibson/man/frost_assembler.Rd
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
  suppressPackageStartupMessages(require(xml2))
  suppressPackageStartupMessages(require(sp))
  suppressPackageStartupMessages(require(rgdal))
#
#------------------------------------------------------------------------------
# Checks
  if (is.null(oldElementCodes)) return(NULL)
  ElCodes<-frost_translate_oldElementCodes(oldElementCodes)
  if (any(ElCodes$elementId=="")) {
    print("ERROR at least one oldElementCodes is not available in gibson")
    print(ElCodes$oldElementCodes[which(ElCodes$elementId=="")])
    return(NULL)
  }
  if ( any(ElCodes$timeResolution=="P1D") &
       any(ElCodes$timeResolution=="PT1H") |
       any(ElCodes$timeResolution!="PT1H" & 
           ElCodes$timeResolution!="P1D") ) {
    print("ERROR it is not possible to mix data with hourly and daily sampling")
    print("      OR it has been specified a non-valid aggregation time")
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
  noldElementCodes<-length(oldElementCodes)
#------------------------------------------------------------------------------
  # Retrieve data and metadata from the Frost API
  if (verbose) print("gibson_fmi_opendata")
  res<-gibson_fmi_opendata(apiKey=apiKey,
                           frost_oldElementCodes=oldElementCodes,
                           timeResolution=ElCodes$timeResolution[1],
                           start_date=start_date,
                           stop_date=stop_date,
                           format=format,
                           spatial_extent=spatial_extent,
                           stationholders=stationholders,
                           stationholders.exclude=stationholders.exclude,
                           WMOonly=WMOonly,
                           WMOin=WMOin,
                           try.again=3,
                           sleep_sec=5,
                           na.rm=T,
                           url.show=F,
                           url4stnlist=url4stnlist)
  if (is.null(res)) {
    print("ERROR while retriving data")
    return(NULL)
  }
  if (length(res$fmi_opendata_data$sourceId)==0) {
    print("WARNING No data")
    return(NULL)
  }
#
#------------------------------------------------------------------------------
# spatial coordinates
  if (verbose) print("spatial coordinates")
  ncrs<-length(coords$x)
  ncoords<-2*length(coords$x)
  nval<-length(res$fmi_opendata_data$lon)
  if (nval>0) {
    coords.val<-as.data.frame(array(data=NA,dim=c(nval,ncoords)))
    # frost eturns coordinates as long-lat
    xy.from<-data.frame(x=res$fmi_opendata_data$lon,
                        y=res$fmi_opendata_data$lat)
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
    print("no metadata from frost")
  }
#
#------------------------------------------------------------------------------
# create the timestamp out of timeResolution,timeOffset,referenceTime
  if (verbose) print("create timeStamp")
  timestamp<-as.POSIXlt(res$fmi_opendata_data$time,
                        origin="1970-01-01", 
                        tz="UTC")
  if (any(is.na(timestamp))) {
    print("WARNING some observations did not have a valid timestamp")
  }
#
#------------------------------------------------------------------------------
# assembly output
  if (verbose) print("assembly  output")
  for (v in 1:noldElementCodes) {
    if (v==1) {
      oEstr<-c(oldElementCodes[1],paste(oldElementCodes[1],"_qcode",sep=""))
    } else {
      oEstr<-c(oEstr,
               oldElementCodes[v],paste(oldElementCodes[v],"_qcode",sep=""))
    }
  }
  nout<-length(timestamp)
  value_qcode<-array(data=NA,dim=c(nmeta_tmp,(2*noldElementCodes)))
  for (v in 1:noldElementCodes) {
    value_qcode[,(2*(v-1)+1):(2*v)]<-
     cbind(res$frost_data$value[ix_tmp[match[nonas]]],
           res$frost_data$qcode[ix_tmp[match[nonas]]])
  }

  out<-data.frame(Rdate2str(timestamp,format=formatOUT),
                  res$fmi_opendata_data$sourceId,
                  coords.val[1:nout,],
                  res$fmi_opendata_data$z,
                  value_qcode,
                  stringsAsFactors=F)
  names(out)<-c("timestamp",
                "sourceId",
                costr,
                "z",
                oEstr)
  return(out)
}
