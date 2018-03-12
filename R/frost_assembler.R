#+ get in-situ observations from frost.met.no
`frost_assembler`<-function(client_id=NULL,
                            oldElementCodes=NULL,
                            start_date=NULL,
                            stop_date=NULL,
                            format="%Y-%m-%dT%H:%M",
                            formatOUT="%Y-%m-%dT%H",
                            countries="NO",
                            spatial_extent=c(4,34,54,72),
                            stationholders=NULL,
                            stationholders.exclude=F,
                            WMOonly=F,
                            na.rm=T,
                            url.show=F,
                            coords=data.frame(x="lon",y="lat",
                                   proj4="+proj=longlat +datum=WGS84",
                                   stringsAsFactors=F),
                            verbose=F
                            )
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
  suppressPackageStartupMessages(require(jsonlite))
  suppressPackageStartupMessages(require(sp))
  suppressPackageStartupMessages(require(rgdal))
#
#------------------------------------------------------------------------------
# Checks
  if (is.null(oldElementCodes)) return(NULL)
  if (any(oldElementCodes %in% c("RR_1","TA")) &
      any(oldElementCodes %in% c("RR","TAM","TAMRR"))) {
    print("ERROR it is not possible to mix data with hourly and daily sampling")
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
  if (verbose) print("gibson_frost")
  res<-gibson_frost(client_id=client_id,
                    oldElementCodes=oldElementCodes,
                    elementId=NULL,
                    timeOffset=NULL,
                    timeResolution=NULL,
                    level.value=NULL,
                    level.levelType=NULL,
                    sources="ALL",
                    start_date=start_date,
                    stop_date=stop_date,
                    format=format,
                    countries=countries,
                    spatial_extent=spatial_extent,
                    stationholders=stationholders,
                    stationholders.exclude=stationholders.exclude,
                    doit.meta=T,
                    doit.data=T,
                    WMOonly=WMOonly,
                    try.again=3,
                    sleep_sec=5,
                    na.rm=na.rm,
                    url.show=url.show)
  if (is.null(res)) {
    print("ERROR while retriving data")
    return(NULL)
  }
  if (length(res$frost_data$sourceId)==0) {
    print("WARNING No data")
    return(NULL)
  }
#
#------------------------------------------------------------------------------
# remove duplicates, where needed
  if (verbose) print("remove duplicates")
  for (i in 1:length(oldElementCodes)) {
    ix<-which(res$frost_data$oldElementCodes==oldElementCodes[i])
    if (length(ix)==0) next
    ixNoDup_tmp<-frost_removeDuplicates(oldElementCodes[i],
              frost_data=data.frame(
               sourceId=res$frost_data$sourceId[ix],
               referenceTime=res$frost_data$referenceTime[ix],
               value=res$frost_data$value[ix],
               qcode=res$frost_data$qcode[ix],
               stringsAsFactors=F) )
    if (exists("ixNoDup")) {
      ixNoDup<-c(ixNoDup,ixNoDup_tmp)
    } else {
      ixNoDup<-ixNoDup_tmp
    }
  }
  if (!exists("ixNoDup")) {
    print("WARNING No data (after duplicate removal)")
    return(NULL)
  }
  rm(ix,ixNoDup_tmp)
  if (length(res$frost_data$value)!=length(ixNoDup)) {
    res$frost_data<-data.frame(res$frost_data$elementId[ixNoDup],
                               res$frost_data$sourceId[ixNoDup],
                               res$frost_data$referenceTime[ixNoDup], 
                               res$frost_data$value[ixNoDup],
                               res$frost_data$qcode[ixNoDup],
                               res$frost_data$timeOffset[ixNoDup],
                               res$frost_data$timeResolution[ixNoDup],
                               res$frost_data$level[ixNoDup],
                               res$frost_data$levelType[ixNoDup],
                               res$frost_data$oldElementCodes[ixNoDup],
                               stringsAsFactors=F)
    names(res$frost_data)<-c("elementId",
                             "sourceId",
                             "referenceTime",
                             "value",
                             "qcode",
                             "timeOffset",
                             "timeResolution",
                             "level",
                             "levelType",
                             "oldElementCodes")

  }
#
#------------------------------------------------------------------------------
# spatial coordinates
  if (verbose) print("spatial coordinates")
  ncrs<-length(coords$x)
  ncoords<-2*length(coords$x)
  nval<-length(res$frost_meta$lon)
  if (nval>0) {
    coords.val<-as.data.frame(array(data=NA,dim=c(nval,ncoords)))
    # frost eturns coordinates as long-lat
    xy.from<-data.frame(x=res$frost_meta$lon,
                        y=res$frost_meta$lat)
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
  timestamp<-structure( rep(NA_real_,length(res$frost_data$value)),
                        class=c("POSIXct", "POSIXt") )
  noldElementCodes<-length(oldElementCodes)
  for (i in 1:noldElementCodes) {
    ix<-which(res$frost_data$oldElementCodes==oldElementCodes[i])
    if (length(ix)==0) next
    timestamp[ix]<-frost_create_ObsTimestamp(oldElementCodes[i],
              frost_data=data.frame(
               referenceTime=res$frost_data$referenceTime[ix],
               timeOffset=res$frost_data$timeOffset[ix],
               stringsAsFactors=F) )
  }
  rm(ix)
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
  unit<-ifelse(oldElementCodes[1] %in% c("RR_1","TA"),"hours","days")
  timestampSeq<-createTimeSeq(start_date=start_date,
                              stop_date=stop_date,
                              format=format,
                              time_step=1,
                              unit=unit,
                              RdateOnlyOut=T)
  for (t in 1:length(timestampSeq)) {
    ix_tmp<-integer(0)
    for (v in 1:noldElementCodes)
      ix_tmp<-c(ix_tmp,which(timestamp==timestampSeq[t] & 
                           res$frost_data$oldElementCodes==oldElementCodes[v]))
    if (length(ix_tmp)==0) next
    ix<-unique(ix_tmp)
    rm(ix_tmp)
    match<-match(res$frost_meta$sourceId,res$frost_data$sourceId[ix])
    rm(ix)
    nonas<-which(!is.na(match))
    if (length(nonas)==0) next
    nmeta_tmp<-length(nonas)
    meta_tmp<-data.frame(
               timestamp=rep(Rdate2str(timestamp[t],format=formatOUT),nmeta_tmp),
               sourceId=res$frost_meta$sourceId[match[nonas]],
               z=res$frost_meta$z[match[nonas]],
               stringsAsFactors=F)
    coords_tmp<-coords.val[match[nonas],]
    rm(match,nonas)
    value_qcode<-array(data=NA,dim=c(nmeta_tmp,(2*noldElementCodes)))
    ix_tmp<-integer(0)
    for (v in 1:noldElementCodes) {
      ix_tmp<-c(ix_tmp,which(timestamp==timestampSeq[t] & 
                           res$frost_data$oldElementCodes==oldElementCodes[v]))
      match<-match(meta_tmp$sourceId,res$frost_data$sourceId[ix_tmp])
      nonas<-which(!is.na(match))
      if (length(nonas)==0) next
      value_qcode[match[nonas],(2*(v-1)+1):(2*v)]<-
       cbind(res$frost_data$value[ix_tmp[match[nonas]]],
             res$frost_data$qcode[ix_tmp[match[nonas]]])
    }
    rm(ix_tmp,match,nonas)
    out_tmp<-data.frame(
            meta_tmp$timestamp,
            meta_tmp$sourceId,
            coords_tmp,
            meta_tmp$z,
            value_qcode,
            stringsAsFactors=F)
    names(out_tmp)<-c("timestamp",
                      "sourceId",
                      costr,
                      "z",
                      oEstr)
    if (exists("out")) {
      out<-rbind(out,out_tmp)
    } else {
      out<-out_tmp
    }
    rm(out_tmp,meta_tmp)
  }    
#------------------------------------------------------------------------------
save(file="tmp",list=c("out"))
  if (exists("out")) {
    return(out)
  } else {
    return(NULL)
  }
}
