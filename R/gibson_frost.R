#+ get in-situ observations from frost.met.no
`gibson_frost`<-function(client_id=NULL,
                         oldElementCodes=NULL,
                         elementId=NULL,
                         timeOffset=NULL,
                         timeResolution=NULL,
                         level.value=NULL,
                         level.levelType=NULL,
                         sources="ALL",
                         start_date=NULL,
                         stop_date=NULL,
                         format="%Y-%m-%dT%H:%M",
                         countries="NO",
                         spatial_extent=c(4,34,54,72),
                         stationholders=NULL,
                         stationholders.exclude=F,
                         doit.meta=T,
                         doit.data=T,
                         WMOonly=F,
                         WMOin=T,
                         try.again=1,
                         sleep_sec=5,
                         na.rm=T,
                         url.show=F)
{
#------------------------------------------------------------------------------
# Documentation: see help(gibson_frost) on R or gibson/man/gibson_frost.Rd
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
#
#------------------------------------------------------------------------------
# Checks
  if (is.null(client_id)) {
    print("ERROR you are required to specify a client_id")
    print("see https://frost.met.no/concepts#getting_started")
    return(NULL)
  }
  if (is.null(start_date) | is.null(stop_date)) {
    print("ERROR dates are needed")
    print("https://frost.met.no/concepts#time_specification")
    return(NULL)
  }
  if (is.null(sources)) {
    print("ERROR sources cannot be set to NULL")
    return(NULL)
  }
  if (!doit.meta & (any(is.na(sources)) | sources=="ALL") ) {
    print("ERROR when doit.meta=F, the sources must be given")
    return(NULL)
  }
  if ( !is.null(oldElementCodes) & 
       ( !is.null(elementId) | 
         !is.null(timeOffset) |
         !is.null(timeResolution) |
         !is.null(level.value) |
         !is.null(level.levelType)) ) {
    print(paste("ERROR you are allowed to specify either oldElementCodes OR",
          " elementId,timeOffset,timeResolution,level.value,level.levelType "))
    return(NULL)
  }
#
#------------------------------------------------------------------------------
# string initialization
  str0<-paste("https://",client_id,"@frost.met.no",sep="")
  if (!is.null(spatial_extent))
#    polygonstr<-paste("geometry=POLYGON%20((",
#                      spatial_extent[1],"%20",spatial_extent[3],",",
#                      spatial_extent[1],"%20",spatial_extent[4],",",
#                      spatial_extent[2],"%20",spatial_extent[4],",",
#                      spatial_extent[2],"%20",spatial_extent[3],",",
#                      spatial_extent[1],"%20",spatial_extent[3],"))",
#                      sep="")
    polygonstr <- ""
  # Weather and Climate Elements
  if (!is.null(oldElementCodes)) {
    ElCodes<-frost_translate_oldElementCodes(oldElementCodes)
    if (any(ElCodes$elementId=="")) {
      print("ERROR at least one oldElementCodes is not available in gibson")
      print(ElCodes$oldElementCodes[which(ElCodes$elementId=="")])
      return(NULL)
    }
  } else {
    ElCodes<-data.frame(elementId,
                        timeOffset,
                        timeResolution,
                        level.value,
                        level.levelType,
                        stringsAsFactors=F)  
  }
  # replace white spaces with %20, so that url works 
  elementIdMod<-gsub(" ","%20",ElCodes$elementId)
  elementIdstr<-paste("elements=",
                      paste(unique(elementIdMod),collapse=","),sep="")
  timeOffsetstr<-paste("timeoffsets=",
                       paste(unique(ElCodes$timeOffset),collapse=","),sep="")
  timeResolutionstr<-paste("timeresolutions=",
                           paste(unique(ElCodes$timeResolution),collapse=","),sep="")
  level.valuestr<-ifelse(any(is.na(ElCodes$level.value)),
   "",paste("&levels=",paste(unique(ElCodes$level.value),collapse=","),sep=""))
  level.levelTypestr<-ifelse(any( is.na(ElCodes$level.levelType) | 
                                  ElCodes$level.levelType==""),
   "",paste("&levelTypes=",paste(unique(ElCodes$level.levelType),collapse=","),sep=""))
  # play with dates so that frost is happy
  formatFrost<-"%Y-%m-%dT%H:%M"
  formatFrost_validtime<-"%Y-%m-%d"
  if (format!=formatFrost) {
    Rdate_start<-as.POSIXlt(str2Rdate(start_date,format=format))
    Rdate_stop<-as.POSIXlt(str2Rdate(stop_date,format=format))
    start_dateMod<-Rdate2str(Rdate_start,formatFrost)
    stop_dateMod<-Rdate2str(Rdate_stop,formatFrost)
  } else {
    start_dateMod<-start_date
    stop_dateMod<-stop_date
  }
  datestr<-paste("referencetime=",start_dateMod,"/",stop_dateMod,sep="")
  start_validtime<-Rdate2str(Rdate_start,formatFrost_validtime)
  stop_validtime<-Rdate2str(Rdate_stop,formatFrost_validtime)
  validtime<-paste("validtime=",start_validtime,"/",stop_validtime,sep="")
#------------------------------------------------------------------------------
# >> METADATA <<
  #............................................................................
  # ==> retrieve station information
  # == query sources@frost ==
  # Supported fields: wmoid, shipcode, name, country, county, validtime, 
  #  externalid, fields, wigosid, ids, municipality, stationholder, icaocode, 
  #  types, geometry
  # 
  # retrieve metadata
  if (doit.meta) {
    str1<-paste(str0,
                "/sources/v0.jsonld?types=SensorSystem",
                "&fields=id,geometry,masl,stationholders,wmoid",
                "&",validtime,
                sep="")
#    if (!is.null(spatial_extent)) str1<-paste(str1,"&",polygonstr,sep="")
    # case of all stations are requested
    if (sources=="ALL") {
      # query by country
      if (!is.null(countries)) {
        for (i in 1:length(countries)) {
          url<-paste(str1,"&country=",countries[i],sep="")
          if (url.show) print(url)
          for (k in 1:try.again) {
            xs<-try(fromJSON(url,flatten=T))
            if (class(xs)!="try-error") break
            # Error 404 means no data in KDVH
            if (regexpr(pattern="HTTP error 404",attr(xs,"condition"))[1]>0) break
            Sys.sleep(sleep_sec)
          }
          # ERROR: frost is not happy with our request, or it is in a bad mood
          if (class(xs)=="try-error") { 
            if (regexpr(pattern="HTTP error 404",attr(xs,"condition"))[1]>0) next
            print("HTTP error 404")
            return(NULL)
          }
          # proceed only if we got some data
          print( paste( " #items from DB=",xs$totalItemCount))
          if (xs$totalItemCount>0) {
            # get (lon,lat) as a vector instead of dealing with a list
            xy<-t(apply(cbind(xs$data$geometry.coordinates,1:xs$totalItemCount),
                        MARGIN=1,
                        FUN=function(x) if(!is.null(x[[1]])) 
                                          as.vector(x[[1]]) 
                                        else c(NA,NA) ))
            # NOTE: this is not actually needed, I've added it when the POLYGON
            #       option was not working for me
            # select only (WMO) station within the region specified
            sel<-vector(mode="logical",length=xs$totalItemCount)
            sel[]<-T
            if (WMOonly) {
              sel<-!is.na(xs$data$wmoId)
            } else {
              if (!WMOin) sel<-sel & is.na(xs$data$wmoId)
            }
            print( paste( " #items after WMO selection=",length(which(sel))))
            sel<-sel & !is.na(xy[,1]) & !is.na(xy[,2]) & !is.na(xs$data$masl)
            if (!is.null(spatial_extent)) {
              ix<-which( xy[,1]>=spatial_extent[1] & 
                         xy[,1]<=spatial_extent[2] &
                         xy[,2]>=spatial_extent[3] &
                         xy[,2]<=spatial_extent[4] &
                         sel )
            } else {
              ix<-which(sel)
            }
            print( paste( " #items after valid coordinates checks=",length(which(sel))))
            if (length(ix)==0) next
            # station holders selection over the region of interest
            frost_e<-new.env()
            frost_e$tmphold<-list()
            devnull<-apply(cbind(xs$data$stationHolders,1:xs$totalItemCount),
                           MARGIN=1,
                           FUN=function(x){
                            if (x[2]%in%ix) 
                             frost_e$tmphold[[which(ix==x[2])]]<-x[[1]]
                           })
            rm(devnull)
              #update global data structure given the results for this country
            if (!exists("metaStat")) {
              metaStat<-data.frame(id=xs$data$id[ix],
                                   lon=xy[ix,1],
                                   lat=xy[ix,2],
                                   z=xs$data$masl[ix],
                                   wmoid=xs$data$wmoId[ix],
                                   stringsAsFactors=F)
              sthold<-frost_e$tmphold
            } else {
              metaStat<-rbind(metaStat,
                             data.frame(id=xs$data$id[ix],
                                        lon=xy[ix,1],
                                        lat=xy[ix,2],
                                        z=xs$data$masl[ix],
                                        wmoid=xs$data$wmoId[ix],
                                        stringsAsFactors=F))
              sthold<-c(sthold,frost_e$tmphold)
            }
            rm(frost_e)
          } # end IF proceed only if we got some data
          rm(xs)
        } #end FOR cycle over countries
      # generic query
      } else {
        url<-str1
        if (url.show) print(url)
        for (k in 1:try.again) {
          xs<-try(fromJSON(url,flatten=T))
          if (class(xs)!="try-error") break
          Sys.sleep(sleep_sec)
        }
        # ERROR: frost is not happy with our request, or it is in a bad mood
        if (class(xs)=="try-error") { print("1. class xs try-error");  return(NULL)}
        if (!exists("xs")) { print("!exists(\"xs\")"); return(NULL)}
        # proceed only if we got some data
        if (xs$totalItemCount>0) {
        # get (lon,lat) as a vector instead of dealing with a list
          xy<-t(apply(cbind(xs$data$geometry.coordinates,1:xs$totalItemCount),
                      MARGIN=1,
                      FUN=function(x) if(!is.null(x[[1]])) 
                                        as.vector(x[[1]]) 
                                      else c(NA,NA) ))
          # select only (WMO) station within the region specified
          sel<-vector(mode="logical",length=xs$totalItemCount)
          sel[]<-T
          if (WMOonly) sel<-is.na(xs$data$wmoId)
          sel<-!is.na(xy[,1]) & !is.na(xy[,2]) & !is.na(xs$data$masl)
          if (!is.null(spatial_extent)) {
            ix<-which( xy[,1]>=spatial_extent[1] & 
                       xy[,1]<=spatial_extent[2] &
                       xy[,2]>=spatial_extent[3] &
                       xy[,2]<=spatial_extent[4] &
                       sel )
          } else {
            ix<-which(sel)
          }
          # data frame as id,lon,lat,elev,wmoid
          metaStat<-data.frame(xs$data$id[ix],
                               xy[ix,],
                               xs$data$masl[ix],
                               xs$data$wmoId[ix],
                               stringsAsFactors=F)
          names(metaStat)<-c("id","lon","lat","z","wmoid")
          # station holders selection over the region of interest
          frost_e<-new.env()
          frost_e$tmphold<-list()
          devnull<-apply(cbind(xs$data$stationHolders,1:xs$totalItemCount),
                         MARGIN=1,
                         FUN=function(x){
                          if (x[2]%in%ix) 
                           frost_e$tmphold[[which(ix==x[2])]]<-x[[1]]
                         })
          rm(devnull)
          sthold<-frost_e$tmphold
          rm(frost_e)
        }
        rm(xs)
      } # end IF query for metadata 
    # sources are specified as a vector of characters
    } else {
      sourcesstr<-paste("&sources=",paste(unique(sources),collapse=","),sep="")
      url<-paste(str1,sources,sep="")
      if (url.show) print(url)
      for (k in 1:try.again) {
        xs<-try(fromJSON(url,flatten=T))
        if (class(xs)!="try-error") break
        Sys.sleep(sleep_sec)
      }
      # ERROR: frost is not happy with our request, or it is in a bad mood
      if (class(xs)=="try-error") { print("2. class xs try-error");  return(NULL)}
      # proceed only if we got some data
      if (xs$totalItemCount>0) {
        # get (lon,lat) as a vector instead of dealing with a list
        xy<-t(apply(cbind(xs$data$geometry.coordinates,1:xs$totalItemCount),
                    MARGIN=1,
                    FUN=function(x) if(!is.null(x[[1]])) 
                                      as.vector(x[[1]]) 
                                    else c(NA,NA) ))
        # select only (WMO) station within the region specified
        sel<-vector(mode="logical",length=xs$totalItemCount)
        sel[]<-T
        if (WMOonly) sel<-is.na(xs$data$wmoId)
        sel<-!is.na(xy[,1]) & !is.na(xy[,2]) & !is.na(xs$data$masl)
        if (!is.null(spatial_extent)) {
          ix<-which( xy[,1]>=spatial_extent[1] & 
                     xy[,1]<=spatial_extent[2] &
                     xy[,2]>=spatial_extent[3] &
                     xy[,2]<=spatial_extent[4] &
                     sel )
        } else {
          ix<-which(sel)
        }
        # data frame as id,lon,lat,elev,wmoid
        metaStat<-data.frame(xs$data$id[ix],
                             xy[ix,],
                             xs$data$masl[ix],
                             xs$data$wmoId[ix],
                             stringsAsFactors=F)
        names(metaStat)<-c("id","lon","lat","z","wmoid")
        # station holders selection over the region of interest
        frost_e<-new.env()
        frost_e$tmphold<-list()
        devnull<-apply(cbind(xs$data$stationHolders,1:xs$totalItemCount),
                       MARGIN=1,
                       FUN=function(x){
                        if (x[2]%in%ix) 
                         frost_e$tmphold[[which(ix==x[2])]]<-x[[1]]
                       })
        rm(devnull)
        sthold<-frost_e$tmphold
        rm(frost_e)
      }
      rm(xs)
    } # end IF sources==ALL | sources are specified
    # ERROR: for some reasons we always got 0 data from frost
    if (!exists("metaStat")) { print("!exists(\"metaStat\")"); return(NULL)}
    nsou<-length(metaStat$id)
    sourcesstr<-paste("&sources=", paste(unique(metaStat$id),collapse=","),sep="")
    print( paste( " #items metaStat=",length(metaStat$id)))
    print( paste( " #items unique metaStat=",length(unique(metaStat$id))))
    #
    # selection based on the station holder
    if (!is.null(stationholders)) {
      match<-vector(mode="numeric",length=nsou)
      match[]<-NA
      if (stationholders.exclude) {
        for (i in 1:nsou) if (!any(sthold[[i]] %in% stationholders)) match[i]<-i
      } else {
        for (i in 1:nsou) if ( any(sthold[[i]] %in% stationholders)) match[i]<-i
      }
      if (!any(!is.na(match))) {
        print("no data available for the stationholder(s) selected")
        return(NULL)
      }
      ix<-which(!is.na(match))
      print( paste( " #items after station holder selection=",length(ix)))
      metaStattmp<-metaStat
      rm(metaStat)
      metaStat<-data.frame(metaStattmp$id[match[ix]],
                           metaStattmp$lon[match[ix]],
                           metaStattmp$lat[match[ix]],
                           metaStattmp$z[match[ix]],
                           metaStattmp$wmoid[match[ix]],
                           stringsAsFactors=F)
      names(metaStat)<-c("id","lon","lat","z","wmoid")
      rm(metaStattmp)
      nsou<-length(metaStat$id)
      stholdtmp<-sthold
      sthold<-list()
      for (i in 1:nsou) sthold[[i]]<-stholdtmp[[match[ix[i]]]]
      rm(stholdtmp)
    }
    print( paste( " #items afterstation holder selection metaStat=",length(unique(metaStat$id))))
  #
  #............................................................................
  # ==> retrieve sensor information
  # == query observations/availableTimeSeries@frost ==
  # Supported fields: elements, performancecategories, exposurecategories, 
  #  levels, timeresolutions, levelUnits, levelTypes, referencetime, 
  #  fields, sources, timeoffsets
  #NOTE: What happens when more than one weather element is provided? 
  # Not sure, but I have the impression frost returns the union of those
  # stations having at least one of the elements (and not the intersection)
  #NOTE: we may have more than one sensor observing a weather element at
  #      the same station. For this reason, the sourceId (unique key) is
  #      formatted as SNxx..x:y where: SNxx..x=id as in metaStat, y c(0,1,...)
  #      sensor number (0 indicates the first sensor)
    url<-paste(str0,
               "/observations/availableTimeSeries/v0.jsonld?",
  #             "fields=sourceId,performanceCategory,exposureCategory,",
  #             "timeOffset,timeResolution",
               elementIdstr,
               "&",timeOffsetstr,
               "&",timeResolutionstr,
               level.valuestr,
               level.levelTypestr,
               "&",datestr,
               sep="")
    if (url.show) print(url)
    for (k in 1:try.again) {
      xs<-try(fromJSON(url,flatten=T))
      if (class(xs)!="try-error") break
      Sys.sleep(sleep_sec)
    }
    # ERROR: frost is not happy with our request, or it is in a bad mood
    if (class(xs)=="try-error") { print("3. class xs try-error");  return(NULL)}
    print( paste( " #items sensors xs$totalItemCount=",xs$totalItemCount))
    # proceed only if we got some data
    if (xs$totalItemCount>0) {
      sourcesaux<-vector()
      sensIdaux<-vector()
      for (i in 1:xs$totalItemCount) {
        sourcesaux[i]<-strsplit(xs$data$sourceId,":")[[i]][1]
        sensIdaux[i]<-strsplit(xs$data$sourceId,":")[[i]][2]
      }
      # check if there is an intersection between the two sets of 
      #  (i) selected stations (metaStat) and (ii) available sensors
      if (any(!is.na(match(sourcesaux,metaStat$id)))) {
        match<-match(sourcesaux,metaStat$id)
        ix<-which(!is.na(match))
        frost_meta<-data.frame(sourcesaux[ix],
                               sensIdaux[ix],
                               xs$data$sourceId[ix],
                               xs$data$performanceCategory[ix],
                               xs$data$exposureCategory[ix],
                               metaStat$lon[match[ix]],
                               metaStat$lat[match[ix]],
                               metaStat$z[match[ix]],
                               stringsAsFactors=F)
        names(frost_meta)<-c("source",
                             "sensId",
                             "sourceId",
                             "performanceCategory",
                             "exposureCategory",
                             "lon",
                             "lat",
                             "z")
        print( paste( " #items intersection sensors/stations=",length(ix)))
      } else {
        print(paste("no data available: the two sets of (i) selected stations",
                    "and (ii) available sensors did not match"))
        return(NULL)
      }
    } else {
      print("no data available for the Welement/time selected")
      return(NULL)
    }
    # remove duplicates
    dupflag<-duplicated(frost_meta)
    if (any(dupflag)) {
      aux<-frost_meta
      rm(frost_meta)
      ix<-which(!dupflag)
      frost_meta<-data.frame(aux$source[ix],
                             aux$sensId[ix],
                             aux$sourceId[ix],
                             aux$performanceCategory[ix],
                             aux$exposureCategory[ix],
                             aux$lon[ix],
                             aux$lat[ix],
                             aux$z[ix],
                             stringsAsFactors=F)
      names(frost_meta)<-c("source",
                           "sensId",
                           "sourceId",
                           "performanceCategory",
                           "exposureCategory",
                           "lon",
                           "lat",
                           "z")
      rm(aux)
    }
    nsouId<-length(frost_meta$sensId)
    # set the list of station holders so to match the sensor list
    tmp<-list()
    for (i in 1:nsouId) {
      ix<-which(metaStat$id==frost_meta$source[i])
      if (length(ix)!=1) {
        if (length(ix)==0) {
          print(paste("WARNING not possible to find station holders for",
                      "source",frost_meta$source[i]))
          tmp[[i]]<-NULL
        } else {
          print(paste("WARNING duplicate sets of station holders for",
                      "source",frost_meta$source[i],
                      "we use just one of these sets"))
          tmp[[i]]<-sthold[[ix[1]]]
        }
      } else {
        tmp[[i]]<-sthold[[ix]]
      }
    }
    rm(sthold)
    sthold<-tmp
    rm(tmp)  
    #
    rm(xs) 
  # in case no metadata are required
  } else {
    frost_meta<-NULL
    sthold<-NULL
  }
#------------------------------------------------------------------------------
# >> DATA <<
# == query observations@frost ==
# supported fields (mandatory): value, referenceTime
# Supported fields (optional): timeResolution, qualityCode, codeTable, 
#  elementId, unit, performanceCategory, exposureCategory, timeOffset, 
#  geometry, sourceId, dataVersion, level; 
  if (doit.data) {
#+ update variables in the frost_e environment
update_frost_e<-function(x){
  if (length(x[[1]])==0) return()
  l<-as.numeric(x[2])
  for (i in 1:length(x[[1]]$value)) {
    frost_e$i<-frost_e$i+1
    j<-frost_e$i
    frost_e$value_qcode[j,1]<-ifelse(is.null(x[[1]]$value[i]),
                       NA,as.numeric(x[[1]]$value[i]))
    frost_e$value_qcode[j,2]<-ifelse(is.null(x[[1]]$qualityCode[i]),
                       NA,as.numeric(x[[1]]$qualityCode[i]))
    levaux<-ifelse(is.null(x[[1]]$level.value[i]),
             NA,x[[1]]$level.value[i])
    levTaux<-ifelse(is.null(x[[1]]$level.levelType[i]),
              "",x[[1]]$level.levelType[i])
    elIdaux<-ifelse(is.null(x[[1]]$elementId[i]),
              "",x[[1]]$elementId[i])   
    tOffaux<-ifelse(is.null(x[[1]]$timeOffset[i]),
              "",x[[1]]$timeOffset[i])
    tResaux<-ifelse(is.null(x[[1]]$timeResolution[i]),
              "",x[[1]]$timeResolution[i])
    if (!is.na(levaux) & levTaux!="") {
      aux<-levaux==ElCodes$level.value &
           levTaux==ElCodes$level.levelType
    } else {
      aux<-rep(T,length=length(ElCodes$elementId))
    }
    aux<- aux & elIdaux==ElCodes$elementId      &
                tOffaux==ElCodes$timeOffset     &
                tResaux==ElCodes$timeResolution
    aux.nas<-T
    if (na.rm) aux.nas<-!is.na(frost_e$value_qcode[j,1])
    if ( any(aux) & aux.nas) {
      frost_e$posok[j]<-j
      frost_e$soId[j]<-xs$data$sourceId[[l]]
      frost_e$tRef[j]<-xs$data$referenceTime[[l]]
      frost_e$elId[j]<-elIdaux
      frost_e$tOff[j]<-tOffaux
      frost_e$tRes[j]<-tResaux
      if (!is.na(levaux) & levTaux!="") {
        frost_e$lev[j]<-levaux 
        frost_e$levT[j]<-levTaux
      } 
      if (!is.null(oldElementCodes)) {
        k<-which(aux)
        frost_e$oelId[j]<-oldElementCodes[k]
      }
    }
  }
} # END of FUN
    if (is.null(frost_meta) & is.null(sources)) {
      print("ERROR a list of sources is needed to retrieve data")
      return(NULL)
    } else {
      if (!is.null(sources)) {
        if (sources!="ALL") {
          sourcesId<-sources
          rm(sources)
        } else {
          sourcesId<-frost_meta$sourceId
        }
      } else {
        sourcesId<-frost_meta$sourceId
      }
    }
    str1<-paste(str0,
               "/observations/v0.jsonld?",
               "fields=elementId,sourceId,value,referenceTime,qualityCode,",
               "timeResolution,timeOffset,level",
               "&",elementIdstr,
               "&",timeOffsetstr,
               "&",timeResolutionstr,
               level.valuestr,
#               level.levelTypestr,
               "&",datestr,
               sep="")
    sourcesIdstr<-paste("&sources=",
                      paste(unique(sourcesId),collapse=","),sep="")
    url<-paste(str1,
               sourcesIdstr,
               sep="")
    #NOTE: Frost API has a limit of 2048 characters for the url, however
    # fromJSON refuses to work if the url has more than 1000 characters
    # 1000 is hard-coded in fromJSON
    if (nchar(url)>=1000) {
      # choose the correct souIdstep
      souIdstep<-70
      for ( j in 1:100) {
        souIdstep<-ifelse(souIdstep>5,souIdstep-5,souidstep-1)
        if (souIdstep<1) break
        flagOK<-T
        for (i in 1:ceiling(nsouId/souIdstep)) {
          i1<-(i-1)*souIdstep+1
          i2<-min(i*souIdstep,nsouId)
          if (i2<i1) break
          sourcesIdstr1<-paste("&sources=", 
                               paste(unique(sourcesId[i1:i2]),
                               collapse=","),sep="")
          url<-paste(str1,
                     sourcesIdstr1,
                     sep="")
          if (nchar(url)>=1000) flagOK<-F
        }
        if (flagOK) break
      }
      if (souIdstep<1) {
        print("ERROR while preparing the query")
        return(NULL)
      }
      for (i in 1:ceiling(nsouId/souIdstep)) {
        i1<-(i-1)*souIdstep+1
        i2<-min(i*souIdstep,nsouId)
        if (i2<i1) break
        sourcesIdstr1<-paste("&sources=", 
                             paste(unique(sourcesId[i1:i2]),
                             collapse=","),sep="")
        url<-paste(str1,
                   sourcesIdstr1,
                   sep="")
        if (url.show) print(url)
        for (k in 1:try.again) {
          xs<-try(fromJSON(url,flatten=T))
          if (class(xs)!="try-error") break
          # error 404 means no data returned
          if (regexpr(pattern="HTTP error 404",attr(xs,"condition"))[1]>0) break
          Sys.sleep(sleep_sec)
        }
        # ERROR: frost is not happy with our request, or it is in a bad mood
        if (class(xs)=="try-error") {
          # error 404 means no data returned
          if (regexpr(pattern="HTTP error 404",attr(xs,"condition"))[1]>0) next
          return(NULL)
        }
        if (xs$totalItemCount==0) next
        totalItemCount<-0
        for (i in 1:xs$totalItemCount) {
          if ( length( xs$data$observations[[i]]) == 0) next
          totalItemCount<-totalItemCount+length(xs$data$observations[[i]][,1])
        }

        # select observations according to na.rm and weather elements
        frost_e             <- new.env()
        frost_e$value_qcode <- array(data=NA,dim=c(totalItemCount,2))
        frost_e$posok       <- vector(mode="numeric",length=totalItemCount)
        frost_e$elId        <- vector(mode="character",length=totalItemCount)
        frost_e$soId        <- vector(mode="character",length=totalItemCount)
        frost_e$tOff        <- vector(mode="character",length=totalItemCount)
        frost_e$tRes        <- vector(mode="character",length=totalItemCount)
        frost_e$tRef        <- vector(mode="character",length=totalItemCount)
        frost_e$lev         <- vector(mode="numeric",length=totalItemCount)
        frost_e$levT        <- vector(mode="character",length=totalItemCount)
        frost_e$oelId       <- vector(mode="character",length=totalItemCount)
        frost_e$posok[] <- NA
        frost_e$lev[]   <- NA        
        frost_e$levT[]  <- ""
        frost_e$oelId[] <- ""
        frost_e$soId[]  <- ""
        frost_e$tRef[]  <- ""
        frost_e$i       <- 0
        devnull<-apply(cbind(xs$data$observations,1:xs$totalItemCount),
                       MARGIN=1,
                       FUN=update_frost_e)
        rm(devnull)
        ix<-which(!is.na(frost_e$posok))
        if (length(ix)==0) next
        if (!exists("frost_data")) {
          frost_data<-data.frame(frost_e$elId[ix],
                                 frost_e$soId[ix],
                                 frost_e$tRef[ix], 
                                 frost_e$value_qcode[ix,1],
                                 frost_e$value_qcode[ix,2],
                                 frost_e$tOff[ix],
                                 frost_e$tRes[ix],
                                 frost_e$lev[ix],
                                 frost_e$levT[ix],
                                 frost_e$oelId[ix],
                                 stringsAsFactors=F)
        } else {
          frost_data<-rbind(frost_data,
                            data.frame(frost_e$elId[ix],
                                       frost_e$soId[ix],
                                       frost_e$tRef[ix], 
                                       frost_e$value_qcode[ix,1],
                                       frost_e$value_qcode[ix,2],
                                       frost_e$tOff[ix],
                                       frost_e$tRes[ix],
                                       frost_e$lev[ix],
                                       frost_e$levT[ix],
                                       frost_e$oelId[ix],
                                       stringsAsFactors=F) )
        }
        rm(frost_e)
      } # end loop over several queries
      if (!exists("frost_data")) {
        frost_data<-integer(0)
      } else {
        names(frost_data)<-c("elementId",
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
    # all the data retrieved in one shot
    } else {
      if (url.show) print(url)
      for (k in 1:try.again) {
        xs<-try(fromJSON(url,flatten=T))
        if (class(xs)!="try-error") break
        Sys.sleep(sleep_sec)
      }
      # ERROR: frost is not happy with our request, or it is in a bad mood
      if (class(xs)=="try-error") { print("4. class xs try-error");  return(NULL)}
      if (xs$totalItemCount>0) {
        totalItemCount<-0
        for (i in 1:xs$totalItemCount) {
          if ( length( xs$data$observations[[i]]) == 0) next
          totalItemCount<-totalItemCount+length(xs$data$observations[[i]][,1])
        }
        # select observations according to na.rm and weather elements
        frost_e<-new.env()
        frost_e$value_qcode<-array(data=NA,dim=c(totalItemCount,2))
        frost_e$posok<-vector(mode="numeric",length=totalItemCount)
        frost_e$elId<-vector(mode="character",length=totalItemCount)
        frost_e$soId<-vector(mode="character",length=totalItemCount)
        frost_e$tOff<-vector(mode="character",length=totalItemCount)
        frost_e$tRes<-vector(mode="character",length=totalItemCount)
        frost_e$tRef<-vector(mode="character",length=totalItemCount)
        frost_e$lev<-vector(mode="numeric",length=totalItemCount)
        frost_e$levT<-vector(mode="character",length=totalItemCount)
        frost_e$oelId<-vector(mode="character",length=totalItemCount)
        frost_e$posok[]<-NA
        frost_e$lev[]<-NA        
        frost_e$levT[]<-""
        frost_e$oelId[]<-""
        frost_e$soId[]<-""
        frost_e$tRef[]<-""
        frost_e$i<-0
        devnull<-apply(cbind(xs$data$observations,1:xs$totalItemCount),
                       MARGIN=1,
                       FUN=update_frost_e)
        rm(devnull)
        ix<-which(!is.na(frost_e$posok))
        if (length(ix)==0) {
          frost_data<-integer(0)
        } else {
          op <- options(digits.secs = 3)
          frost_data<-data.frame(frost_e$elId[ix],
                                 frost_e$soId[ix],
                                 frost_e$tRef[ix], 
                                 frost_e$value_qcode[ix,1],
                                 frost_e$value_qcode[ix,2],
                                 frost_e$tOff[ix],
                                 frost_e$tRes[ix],
                                 frost_e$lev[ix],
                                 frost_e$levT[ix],
                                 frost_e$oelId[ix],
                                 stringsAsFactors=F)
          names(frost_data)<-c("elementId",
                               "sourceId",
                               "referenceTime",
                               "value",
                               "qcode",
                               "timeOffset",
                               "timeResolution",
                               "level",
                               "levelType",
                               "oldElementCodes")
          rm(frost_e)
        }
      } else {
        frost_data<-integer(0)
      }
      rm(xs)
    } # END of data retrieve (one or more shots)
  # caso of doit.data=F
  } else {
    frost_data<-NULL
  }
  # Normal exit
  return(list(frost_data=frost_data,
              frost_meta=frost_meta,
              stationholders=sthold))
}
