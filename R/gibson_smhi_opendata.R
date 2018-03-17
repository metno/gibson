#+ get in-situ observations from https://opendata-download-metobs.smhi.se/
`gibson_smhi_opendata`<-function(frost_oldElementCodes=NULL,
                                 version="1.0",
                                 station_set_all=T,
                                 latest_hour=T,
                                 try.again=1,
                                 sleep_sec=5,
                                 na.rm=T,
                                 url.show=F)
{
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
# no scientific notation so to get correct unix timestamp
  options(scipen=999)
#
#------------------------------------------------------------------------------
# Checks
  if (!station_set_all | !latest_hour) {
    print("ERROR option not yest implemented")
    return(NULL)
  }
#
#------------------------------------------------------------------------------
# string initialization
  str0<-paste("https://opendata-download-metobs.smhi.se/api/version/",
              version,sep="")
#
#------------------------------------------------------------------------------
  # Weather and Climate Elements
  if (!is.null(frost_oldElementCodes)) {
    parameterIds<-smhi_opendata_translate_oldElementCodes(frost_oldElementCodes)
    if (any(is.na(parameterIds))) {
      print("ERROR at least one frost_oldElementCodes is not available in gibson")
      print(frost_oldElementCodes[which(is.na(parameterIds))])
      return(NULL)
    }
  } else {
    print("ERROR frost_oldElementCodes need to be specified")
    return(NULL)
  }
#
#------------------------------------------------------------------------------
# GET LATEST HOUR DATA
  if (latest_hour) {
    data<-integer(0)
    for (i in 1:length(parameterIds)) {
      url<-paste(str0,"/parameter/",parameterIds[i],
                 "/station-set/all/period/latest-hour/data.json",sep="")
      if (url.show) print(url)
      for (k in 1:try.again) {
        xs<-try(fromJSON(url,flatten=T))
        if (class(xs)!="try-error") break
        # Error 404 means no data
        if (regexpr(pattern="HTTP error 404",attr(xs,"condition"))[1]>0) break
        Sys.sleep(sleep_sec)
      }
      # ERROR: frost is not happy with our request, or it is in a bad mood
      if (class(xs)=="try-error") { 
        if (regexpr(pattern="HTTP error 404",attr(xs,"condition"))[1]>0) next
        return(NULL)
      }
      xs_itemcount<-dim(xs$station)[1]
      if (xs_itemcount==0) next
      if (!exists("meta")) {
        meta<-data.frame(station_key=xs$station$key,
                         lon=xs$station$longitude,
                         lat=xs$station$latitude,
                         z=xs$station$height,
                         owner=xs$station$owner,
                         stringsAsFactors=F)
      } else {
        ix<-which(!(xs$station$key %in% meta$station_key))
        if (length(ix)>0) {
          meta<-rbind(meta,
                      data.frame(station_key=xs$station$key[ix],
                                 lon=xs$station$longitude[ix],
                                 lat=xs$station$latitude[ix],
                                 z=xs$station$height[ix],
                                 owner=xs$station$owner[ix],
                                 stringsAsFactors=F))
        }
      }
      #
      date<-vector(mode="numeric",length=xs_itemcount)
      value<-vector(mode="numeric",length=xs_itemcount)
      quality<-vector(mode="character",length=xs_itemcount)
      posok<-vector(mode="numeric",length=xs_itemcount)
      date[]<-NA
      value[]<-NA
      posok[]<-NA
      quality[]<-""
      for (j in 1:xs_itemcount) {
        if (is.null(xs$station$value[[j]])) {
          if (!na.rm) {
            posok[j]<-j
          }  
        } else {
          posok[j]<-j
          date[j]<-as.numeric(xs$station$value[[j]]$date)
          value[j]<-as.numeric(xs$station$value[[j]]$value)
          quality[j]<-xs$station$value[[j]]$quality
        }
      }
      ix<-which(!is.na(posok))
      if (length(ix)==0) next
      if (any(is.na(date))) date[which(is.na(date))]<-unique(date[which(!is.na(date))])[1]
      n<-length(ix)
      data<-rbind(data,
                  data.frame(parameterId=rep(parameterIds[i],n),
                             station_key=xs$station$key[ix],
                             date=date[ix],
                             value=value[ix],
                             qcode=quality[ix],
                             frost_oldElementCodes=rep(frost_oldElementCodes[i],n), 
                             stringsAsFactors=F))
    } # END loop over parameter Ids
  } # END if latest_hour
  # Normal exit
  return(list(smhi_opendata_data=data,
              smhi_opendata_meta=meta))
}
