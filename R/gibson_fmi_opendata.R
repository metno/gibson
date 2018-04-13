# get in-situ observations from http://data.fmi.fi
`gibson_fmi_opendata`<-function(apiKey=NULL,
                                frost_oldElementCodes=NULL,
                                timeResolution=NULL, # PT1H or P1D
                                start_date=NULL,
                                stop_date=NULL,
                                format="%Y-%m-%dT%H:%M",
                                spatial_extent=c(19.09,59.3,31.59,70.13),
                                stationholders=NULL,
                                stationholders.exclude=F,
                                WMOonly=F,
                                WMOin=T,
                                try.again=1,
                                sleep_sec=5,
                                na.rm=T,
                                url.show=F,
          url4stnlist="http://en.ilmatieteenlaitos.fi/observation-stations") {
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
# no scientific notation so to get correct unix timestamp
  options(scipen=999)
#------------------------------------------------------------------------------
# >> METADATA <<
  #............................................................................
  # ==> retrieve station information
  for (k in 1:try.again) {
    stn_list<-try(fmi_station_list())
    if (class(stn_list)!="try-error") break
    Sys.sleep(sleep_sec)
  }
  if (class(stn_list)=="try-error") return(NULL) 
  # NOTE: (i) list has lat-lon with only two digits,so we drop them
  #           instead, we derive this info from the xml
  #      (ii) this is the only place to get z (elevation)
  metaStat<-data.frame(id=stn_list$fmisid,
                       z=stn_list$elevation,
                       wmo=stn_list$wmo,
                       group=stn_list$group,
                       stringsAsFactors=F)
  rm(stn_list)
  nmeta<-length(metaStat$id)
#------------------------------------------------------------------------------
# >> DATA <<
# == query observations@data.fmi.fi ==
  # get data in xml-format
  partmp<-fmi_opendata_translate_oldElementCodes(oldElementCodes=frost_oldElementCodes)
  if (timeResolution=="PT1H") {
    url<-fmi_getURL(apiKey=apiKey,
                    abbrv="minute",
                    starttime=start_date,
                    endtime=stop_date,
                    format=format,
                    parameters=partmp,
                    bbox=spatial_extent)
  } else if (timeResolution=="P1D") {
    url<-fmi_getURL(apiKey=apiKey,
                    abbrv="day",
                    starttime=start_date,
                    endtime=stop_date,
                    format=format,
                    parameters=partmp,
                    bbox=spatial_extent)
  } else {
    print(paste0("ERROR in gibson_fmi_opendata.R: timeResolution",
                 " must be either \"P1D\" or \"PT1H\""))
    return(NULL)
  }
  if (url.show) print(url)
  xml<-read_xml(url)
  # get metadata from xml (lat/lon/id/wmo)
  tl<-xml_find_all(
       xml_find_all(xml,".//target:LocationCollection"),
       ".//target:Location")
  ntl<-length(tl)
  mp<-xml_find_all(xml_find_all(xml,".//gml:MultiPoint"),".//gml:Point")
  nmp<-length(mp)
  if (ntl!=nmp) 
    print(paste0("WARNING ntl is different from nmp,",
                 " this is not supposed to happen"))
  if (ntl==0) {
    print("gibson_fmi_opendata.R, no data available")
    return(NULL)
  }
  fmisid<-vector(mode="character",length=nmeta)
  name<-vector(mode="character",length=nmeta)
  lat<-vector(mode="numeric",length=nmeta)
  lon<-vector(mode="numeric",length=nmeta)
  wmo<-vector(mode="character",length=nmeta)
  fmisid[]<-NA
  name[]<-NA
  lat[]<-NA
  lon[]<-NA
  wmo[]<-NA
  for (i in 1:ntl) {
    mp.pid<-xml_attr(mp[[i]],"id")
    mp.name<-xml_text(xml_find_all(mp[[i]],".//gml:name"))
    mp.pos<-strsplit(xml_text(xml_find_all(mp[[i]],".//gml:pos"))," ")[[1]]
    tl.fmisid<-xml_text(xml_find_all(tl[[i]],".//gml:identifier"))
    tmp1<-xml_attr(xml_find_all(tl[[i]],".//gml:name"),"codeSpace")
    tmp2<-xml_text(xml_find_all(tl[[i]],".//gml:name"))
    tl.name<-tmp2[which(tmp1=="http://xml.fmi.fi/namespace/locationcode/name")]
    tl.wmo<-tmp2[which(tmp1=="http://xml.fmi.fi/namespace/locationcode/wmo")]
    tl.wmo<-ifelse(tl.wmo=="NaN",NA,tl.wmo)
    if (mp.pid!=paste0("point-",tl.fmisid)) 
      print(paste0("WARNING: mp.pid is different from tl.fmisid,",
                   " this is not supposed to happen"))
    ix<-which(metaStat$id==tl.fmisid)
    if (length(ix)==0) {
      print(paste0("WARNING: found a station id in the xml that is not",
                   " present in the station list,",
                   " this is not supposed to happen"))
      print(paste0("fmisid wmo =",tl.fmisid," ",tl.wmo))
      metaStat<-rbind(metaStat,
                      c(tl.fmisid,NA,tl.wmo))
      nmeta<-nmeta+1
      lat<-c(lat,as.numeric(mp.pos[1]))
      lon<-c(lon,as.numeric(mp.pos[2]))
    } else  {
      if (!is.na(metaStat$wmo[ix])) {
        if (is.na(tl.wmo) | 
            as.numeric(tl.wmo)!=as.numeric(metaStat$wmo[ix])) {
          print(paste0("WARNING: found a wmo station id in the xml that is",
                   " different from the one in the station list,",
                   " this is not supposed to happen"))
          print(paste0("tl.wmo=",tl.wmo))
          print(paste0("metaStat$wmo[ix]=",metaStat$wmo[ix]))
        }
      }
      lat[ix]<-as.numeric(mp.pos[1])
      lon[ix]<-as.numeric(mp.pos[2])
    }
  }
  metaStat$lat<-lat
  metaStat$lon<-lon
  rm(lat)
  rm(lon)
  rm(wmo)
  rm(fmisid)
  # get parameters
  pars <- xml_attr(xml_find_all(xml, ".//swe:field"),attr="name")
  npars<-length(pars)
  pars_old<-fmi_opendata_translate_oldElementCodes(fmi_parameterId=pars)
  # get data
  # position/tcol are used to get lat/lon/time(unix)
  positions<-strsplit(xml_text(xml_find_all(xml,".//gmlcov:positions")),"\n")
  tcol<-strsplit(xml_text(xml_find_all(xml,".//gmlcov:positions")),"\n")
  # datablock/data are used to get the observations
  datablock<-xml_find_all(xml, ".//gml:DataBlock")
  dataxml<-strsplit(xml_text(xml_find_first(datablock,
               ".//gml:doubleOrNilReasonTupleList")),"\n")
  ndata<-length(dataxml[[1]])-2
  data<-array(data=NA,dim=c(ndata,npars))
  time<-vector(mode="numeric",length=ndata)
  time[]<-NA
  lat<-time
  lon<-time
  wmo<-time
  id<-time
  z<-time
  group<-time
  for (j in 2:(ndata+1)) {
    strv<-strsplit(gsub("(?<=[\\s])\\s*|^\\s+|\\s+$",
                        "", dataxml[[1]][j], perl=TRUE),split=" ")
    stra<-strsplit(gsub("(?<=[\\s])\\s*|^\\s+|\\s+$",
                        "", positions[[1]][j], perl=TRUE),split=" ")
    if (length(strv[[1]])==npars) {
      i<-j-1
      latj<-as.numeric(stra[[1]][1])
      lonj<-as.numeric(stra[[1]][2])
      ix<-which(latj==metaStat$lat & lonj==metaStat$lon)
      if (length(ix)!=1) {
        print(paste0("WARNING: found ",length(ix),
              " correspondences while this number should be 1"))
        next
      }
      data[i,]<-as.numeric(strv[[1]])
      time[i]<-as.numeric(stra[[1]][3])
      lat[i]<-latj
      lon[i]<-lonj
      z[i]<-metaStat$z[ix] 
      id[i]<-metaStat$id[ix] 
      wmo[i]<-metaStat$wmo[ix] 
      group[i]<-metaStat$group[ix]
    }
  }
  rm(metaStat)
  data[!is.finite(data)]<-NA
  # apply filters 
  # filter out not 0-minute data so to keep only hourly/daily data
  tt<-as.POSIXlt(time,origin="1970-01-01",tz="UTC")
  flag<-tt$min==0
  if (WMOonly) flag<-flag & !is.na(wmo)
  if (!WMOin) flag<-flag & is.na(wmo)
  if (!is.null(stationholders)) {
    if (stationholders.exclude)  {
      flag<-flag & !(group %in% stationholders)
    } else {
      flag<-flag & (group %in% stationholders)
    }
  }
  ix<-which(flag)
  if (length(ix)>0) {
    fmi_data<-data.frame(time[ix],
                         id[ix],
                         lat[ix],
                         lon[ix],
                         z[ix],
                         wmo[ix],
                         data[ix,],   
                         stringsAsFactors=F)
    names(fmi_data)<-c("timestamp",
                       "sourceId",
                       "lat",
                       "lon",
                       "z",
                       "wmoId",
                       pars_old)
  } else {
    fmi_data<-NULL
  }
  # Normal exit
  return(list(fmi_opendata_data=fmi_data))
}
