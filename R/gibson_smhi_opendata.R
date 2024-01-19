#+ get in-situ observations from https://opendata-download-metobs.smhi.se/
`gibson_smhi_opendata`<-function(frost_oldElementCodes=NULL,
                                 version="1.0",
                                 station_set_all=T,
                                 latest_hour=T,
                                 latest_day=F,
                                 latest_months=F,
                                 try.again=1,
                                 sleep_sec=5,
                                 na.rm=T,
                                 url.show=F,
                                 verbose=T)
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
  if (!(station_set_all & latest_hour) & !latest_months & !latest_day) {
    print("ERROR option not yet implemented")
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
    data <- list()
    data$frost_oldElementCodes <- frost_oldElementCodes
    data$elements <- list()
    for (i in 1:length(parameterIds)) {
      data$elements[[i]] <- list()
      url<-paste0(str0,"/parameter/",parameterIds[i],
                 "/station-set/all/period/latest-hour/data.json",sep="")
      if (url.show) print(url)
      for (k in 1:try.again) {
        xs<-try(fromJSON(url,flatten=T))
        if (class(xs)!="try-error") break
        # Error 404 means no data
        if (regexpr(pattern="HTTP error 404",attr(xs,"condition"))[1]>0) break
        Sys.sleep(sleep_sec)
      }
      # ERROR: smhi is not happy with our request, or it is in a bad mood
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
      data$elements[[i]]$date_time <- numeric(0)
      data$elements[[i]]$data <- list()
      first <- T
      for (j in 1:xs_itemcount) {
        if ( is.null(xs$station$value[[j]]$date)) next
        if ( is.null(xs$station$value[[j]]$value)) next
        if ( is.na( as.numeric( xs$station$value[[j]]$date)) | 
             is.na( as.numeric( xs$station$value[[j]]$value)))
            next
        if (first) {
          data$elements[[i]]$date_time <- as.numeric(xs$station$value[[j]]$date)
          data$elements[[i]]$data[[1]] <- 
              data.frame( parameterId=parameterIds[i],
                          station_key=xs$station$key[j],
                          date=as.numeric(xs$station$value[[j]]$date),
                          value=as.numeric(xs$station$value[[j]]$value),
                          qcode=xs$station$value[[j]]$quality,
                          frost_oldElementCodes=frost_oldElementCodes[i], 
                          stringsAsFactors=F)

          first <- F
        } else {
          data$elements[[i]]$data[[1]] <- rbind( 
              data$elements[[i]]$data[[1]],
              data.frame( parameterId=parameterIds[i],
                          station_key=xs$station$key[j],
                          date=as.numeric(xs$station$value[[j]]$date),
                          value=as.numeric(xs$station$value[[j]]$value),
                          qcode=xs$station$value[[j]]$quality,
                          frost_oldElementCodes=frost_oldElementCodes[i], 
                          stringsAsFactors=F))
        } 
      } # END loop over items
    } # END loop over parameter Ids
  } # END if latest_hour
#
#------------------------------------------------------------------------------
# GET LATEST day/months DATA
  else if (latest_day | latest_months) {
    data <- list()
    data$frost_oldElementCodes <- frost_oldElementCodes
    data$elements <- list()
    for (i in 1:length(parameterIds)) {
      data$elements[[i]] <- list()
      # get metadata  
      url<-paste0(str0,"/parameter/",parameterIds[i],"/station.json")
      if (url.show) print(url)
      for (k in 1:try.again) {
        xs<-try(fromJSON(url,flatten=T))
        if (class(xs)!="try-error") break
        # Error 404 means no data
        if (regexpr(pattern="HTTP error 404",attr(xs,"condition"))[1]>0) break
        Sys.sleep(sleep_sec)
      }
      # ERROR: smhi is not happy with our request, or it is in a bad mood
      if (class(xs)=="try-error") { 
        if (regexpr(pattern="HTTP error 404",attr(xs,"condition"))[1]>0) next
        return(NULL)
      }
      xs_itemcount<-dim(xs$station)[1]
      if ( xs_itemcount == 0) next
      ix_active <- which( xs$station$active)
      xs_itemcount <- length( ix_active)
      if ( xs_itemcount == 0) next
      if (verbose) print(paste("number of active stations is",xs_itemcount))
      if (!exists("meta")) {
        meta<-data.frame(station_key=xs$station$key[ix_active],
                         lon=xs$station$longitude[ix_active],
                         lat=xs$station$latitude[ix_active],
                         z=xs$station$height[ix_active],
                         owner=xs$station$owner[ix_active],
                         stringsAsFactors=F)
      } else {
        ix<-which(!(xs$station$key[ix_active] %in% meta$station_key))
        if (length(ix)>0) {
          meta<-rbind(meta,
                      data.frame(station_key=xs$station$key[ix_active][ix],
                                 lon=xs$station$longitude[ix_active][ix],
                                 lat=xs$station$latitude[ix_active][ix],
                                 z=xs$station$height[ix_active][ix],
                                 owner=xs$station$owner[ix_active][ix],
                                 stringsAsFactors=F))
        }
      }
      stationKeys <- xs$station$key[ix_active]
      #
      data$elements[[i]]$date_time <- numeric(0)
      data$elements[[i]]$data <- list()
      # download data station by station
      for (j in 1:xs_itemcount) {
        if (latest_day) {
          url<-paste0(str0,"/parameter/",parameterIds[i],"/station/",stationKeys[j],
                      "/period/latest-day/data.json")
        } else if (latest_months) {
          url<-paste0(str0,"/parameter/",parameterIds[i],"/station/",stationKeys[j],
                      "/period/latest-months/data.json")
        }
        if (url.show) print(paste(j,"/",xs_itemcount,url))
        for (k in 1:try.again) {
          xsj<-try(fromJSON(url,flatten=T))
          if (class(xsj)!="try-error") break
          # Error 404 means no data
          if (regexpr(pattern="HTTP error 404",attr(xsj,"condition"))[1]>0) break
          Sys.sleep(sleep_sec)
        }
        # ERROR: smhi is not happy with our request, or it is in a bad mood
        if (class(xsj)=="try-error") {
          if (regexpr(pattern="HTTP error 404",attr(xsj,"condition"))[1]>0) next
#          return(NULL)
          next
        }
        if ( length(xsj$value) == 0) next
        xsj_itemcount <- dim(xsj$value)[1]
        if ( xsj_itemcount == 0) next
        if (verbose) print(paste("number of returned items",xsj_itemcount))
        for (k in 1:xsj_itemcount) {
          if ( is.null(xsj$value$date[k])) next
          if ( is.null(xsj$value$value[k])) next
          if ( is.na( as.numeric( xsj$value$date[k])) | 
               is.na( as.numeric( xsj$value$value[k])))
            next
          if (xsj$value$date[k] %in% data$elements[[i]]$date_time) {
            ixk <- which( xsj$value$date[k] == data$elements[[i]]$date_time)
            data$elements[[i]]$data[[ixk]] <- rbind( 
              data$elements[[i]]$data[[ixk]],
              data.frame( parameterId=parameterIds[i],
                          station_key=xsj$station$key,
                          date=as.numeric(xsj$value$date[k]),
                          value=as.numeric(xsj$value$value[k]),
                          qcode=xsj$value$quality[k],
                          frost_oldElementCodes=frost_oldElementCodes[i], 
                          stringsAsFactors=F))
          } else {
            data$elements[[i]]$date_time <- c( data$elements[[i]]$date_time, xsj$value$date[k])
            data$elements[[i]]$data[[length(data$elements[[i]]$date_time)]] <- 
              data.frame( parameterId=parameterIds[i],
                          station_key=xsj$station$key,
                          date=as.numeric(xsj$value$date[k]),
                          value=as.numeric(xsj$value$value[k]),
                          qcode=xsj$value$quality[k],
                          frost_oldElementCodes=frost_oldElementCodes[i], 
                          stringsAsFactors=F)
          }
        }
      }
    } # END loop over parameter Ids
  }
#
#------------------------------------------------------------------------------
  # Normal exit
  return(list(smhi_opendata_data=data,
              smhi_opendata_meta=meta))
}
