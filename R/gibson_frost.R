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
                         formatOUT="%Y-%m-%dT%H",
                         countries="NO",
                         spatial_extent=c(4,34,54,72),
                         stationholders=NULL,
                         stationholders.exclude=F,
                         doit.meta=T,
                         doit.data=T,
                         WMOonly=F,
                         try.again=1,
                         sleep_sec=5,
                         na.rm=T)
{
#------------------------------------------------------------------------------
# Description:
# Frost API retrieve observations and metadata from the Climate Database (KDVH)
# of the Norwegian Meterological Institute (MET Norway or MET.NO).
# Frost documentation is available at frost.met.no.
# The KDVH contains in-situ observations measured by sensors located at stations
# belonging to several different station holders (not only "MET.NO").
# One station can belong to more than one station holder.
# The key used by frost is the data source identifier of an observation.
# From the documentation on frost.met.no:
# "The ID(s) of the data sources to get time series for as a comma-separated 
# list of Frost API station IDs: SN<int>[:<int>|all] 
# (e.g. SN18700, SN18700:0, or SN18700:all). 0 is the main sensor and x≥1 is a 
# parallel sensor."
# The data source identifier is not a unique key.
# The observations measured by the sensors and stored in the KDVH can be  
# post-processed. They can be aggregated in different ways over
# different time intervals (hours/days/months/years/...). They can be 
# quality checked.
# The time stamps associated to the observed values mark the end of the 
# aggregation period. The time zone assumed for the time stamps is UTC.
#  
#
# Input Parameters:
# client_id character string with the API Client ID 
#            (see https://frost.met.no/auth/requestCredentials.html)
# Welements character vector with Weather and Climate Elements Id
#            (see https://frost.met.no/elementtable)           
# sources character vector with the source Ids, use "ALL" to retrieve all the
#         sources within the specified selection. Note that if doit.meta=FALSE
#         then sources="ALL" will generate an error message.
# start_date character string with the time stamp of the first observation 
# stop_date character string with the time stamp of the last observation 
# format charater string specifying the date-time format of start_date and
#        stop_date (see strptime help page)
# formatOUT charater string specifying the date-time format desired for the
#           the output (see strptime help page)
# countries character vector with the abbreviations of the countries. Only
#           data sources located in those countries will be returned.
# spatial_extent numeric vector of the form c(long_min,long_max,lan_min,
#                lan_max) that identifies a rectangle used to select the 
#                data sources. The lower left corner is (long_min,lan_min)
#                the upper right corner is (long_max,lat_max).
# stationholders character vector with the names of the station holders
# stationholders.exclude logical, it is used in combination with stationholders
#                        If FALSE, then the stationholder list will be used 
#                        to select the station holders to include in the
#                        output.
#                        If TRUE, then the stationholder list will be used 
#                        to select the station holders NOT to include in the
#                        output.
# doit.meta logical. If set to TRUE then the source metadata are retrieved
# doit.data logical. If set to TRUE then the observed values are retrieved 
# WMOonly logical, if TRUE then only WMO stations (i.e., having a WMO code
#         will be returned
# try.again numeric value specifying the number of request attemps before
#           giving up
# sleep_sec numeric value, number of seconds to wait between two consecutive 
#           requests
# na.rm logical, if TRUE remove NAs from the output
#
#
# Returned values:
# Only stations with complete (long,lat,elevation) information are returned. 
# 
# Frost API Response Messages:
# HTTP_Status_Code Reason
#  400 	            Invalid parameter value or malformed request.
#  401              Unauthorized client ID.
#  404              No data was found for the list of query Ids.
#  500              Internal server error.
#------------------------------------------------------------------------------
#
  require(jsonlite)
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
  oldElementCodesAvail<-c("RR_1","TA","TAM","TAMRR","RR")
  if ( !is.null(oldElementCodes)) {
    if (any(!oldElementCodes %in% oldElementCodesAvail)) {
      print("ERROR oldElementCodes must be one of")
      print(oldElementCodesAvail)
      return(NULL)
    }
  } 
#------------------------------------------------------------------------------
# string initialization
  str0<-paste("https://",client_id,"@frost.met.no",sep="")
  if (!is.null(spatial_extent))
    polygonstr<-paste("&geometry=POLYGON%20((",
                      spatial_extent[1],"%20",spatial_extent[3],",",
                      spatial_extent[1],"%20",spatial_extent[4],",",
                      spatial_extent[2],"%20",spatial_extent[4],",",
                      spatial_extent[2],"%20",spatial_extent[3],",",
                      spatial_extent[1],"%20",spatial_extent[3],"))",
                      sep="")
  # Weather and Climate Elements
  if (!is.null(oldElementCodes)) {
    elementId<-vector()
    timeOffset<-vector()
    timeResolution<-vector()
    level.value<-vector()
    level.levelType<-vector()
    for (i in 1:length(oldElementCodes)) {
      switch(oldElementCodes[i],
             "RR_1" = {  elementId[i]="sum(precipitation_amount PT1H)"
                         timeOffset[i]="PT06H"
                         timeResolution[i]="PT1H"
                         level.value[i]=NA
                         level.levelType[i]=NA
                      },
               "TA" = {  elementId[i]="air_temperature"
                         timeOffset[i]="PT18H"
                         timeResolution[i]="PT1H"
                         level.value[i]=2
                         level.levelType[i]="height_above_ground"
                      },
              "TAM" = {  elementId[i]="mean(air_temperature P1D)"
                         timeOffset[i]="PT18H"
                         timeResolution[i]="P1D"
                         level.value[i]=2
                         level.levelType[i]="height_above_ground"
                      },
            "TAMRR" = {  elementId[i]="mean(air_temperature P1D)"
                         timeOffset[i]="PT06H"
                         timeResolution[i]="P1D"
                         level.value[i]=2
                         level.levelType[i]="height_above_ground"
                      },
               "RR" = {  elementId[i]="sum(precipitation_amount P1D)"
                         timeOffset[i]="PT06H"
                         timeResolution[i]="P1D"
                         level.value[i]=NA
                         level.levelType[i]=NA
                      }
            )
    }
  }
  # replace white spaces with %20, so that url works 
  elementIdMod<-gsub(" ","%20",elementId)
  elementIdstr<-paste("&elements=",paste(elementIdMod,collapse=","),sep="")
  timeOffsetstr<-paste("&timeoffsets=",paste(timeOffset,collapse=","),sep="")
  timeResolutionstr<-paste("&timeresolutions=",
                           paste(timeResolution,collapse=","),sep="")
  level.valueMod<-level.value
  if (any(is.na(level.value))) 
    level.valueMod<-level.value[which(!is.na(level.value))]
  level.valuestr<-ifelse(length(level.valueMod)>0,
                   paste("&levels=",paste(level.value,collapse=","),sep=""),
                   "")
  level.levelTypeMod<-level.levelType
  if (any(is.na(level.levelType))) 
    level.levelTypeMod<-level.levelType[which(!is.na(level.levelType))]
  level.levelTypestr<-ifelse(length(level.levelTypeMod)>0,
                   paste("&levelTypes=",paste(level.levelType,collapse=","),sep=""),
                   "")
  # play with dates so that frost is happy
  formatFrost<-"%Y-%m-%dT%H:%M"
  if (format!=formatFrost) {
    Rdate_start<-as.POSIXlt(str2Rdate(start_date,format=format))
    Rdate_stop<-as.POSIXlt(str2Rdate(stop_date,format=format))
    start_dateMod<-Rdate2str(Rdate_start,formatFrost)
    stop_dateMod<-Rdate2str(Rdate_stop,formatFrost)
  } else {
    start_dateMod<-start_date
    stop_dateMod<-stop_date
  }
  datestr<-paste("&referencetime=",start_dateMod,"/",stop_dateMod,sep="")
#------------------------------------------------------------------------------
# >> METADATA <<
  #............................................................................
  # ==> retrieve station information
  # == query sources@frost ==
  # Supported fields: wmoid, shipcode, name, country, county, validtime, 
  #  externalid, fields, wigosid, ids, municipality, stationholder, icaocode, 
  #  types, geometry
  if (doit.meta) {
    str1<-paste(str0,
                "/sources/v0.jsonld?types=SensorSystem",
                "&fields=id,geometry,masl,stationholders,wmoid",
                sep="")
    if (!is.null(spatial_extent)) str1<-paste(str1,polygonstr,sep="")
    # case of all stations are requested
    if (sources=="ALL") {
      # query by country
      if (!is.null(countries)) {
        for (i in 1:length(countries)) {
          url<-paste(str1,"&country=",countries[i],sep="")
#          print(url)
          for (k in 1:try.again) {
            xs<-try(fromJSON(url,flatten=T))
            if (class(xs)!="try-error") break
            Sys.sleep(sleep_sec)
          }
          # ERROR: frost is not happy with our request, or it is in a bad mood
          if (class(xs)=="try-error") return(NULL)
          # proceed only if we got some data
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
            # data frame as id,lon,lat,elev,stationholder
            tmp<-data.frame(xs$data$id[ix],
                            xy[ix,],
                            xs$data$masl[ix],
                            xs$data$wmoId[ix],
                            stringsAsFactors=F)
            names(tmp)<-c("id","lon","lat","z","wmoid")
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
              metaStat<-tmp
              sthold<-frost_e$tmphold
            } else {
              metaStat<-rbind(metaStat,tmp)
              sthold<-c(sthold,frost_e$tmphold)
            }
            rm(tmp,frost_e)
          } # end IF proceed only if we got some data
          rm(xs)
        } #end FOR cycle over countries
      # generic query
      } else {
        url<-str1
        print(url)
        for (k in 1:try.again) {
          xs<-try(fromJSON(url,flatten=T))
          if (class(xs)!="try-error") break
          Sys.sleep(sleep_sec)
        }
        # ERROR: frost is not happy with our request, or it is in a bad mood
        if (class(xs)=="try-error") return(NULL)
        if (!exists("xs")) return(NULL)
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
          # data frame as id,lon,lat,elev,stationholder
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
      sourcesstr<-paste("&sources=",paste(sources,collapse=","),sep="")
      url<-paste(str1,sources,sep="")
      for (k in 1:try.again) {
        xs<-try(fromJSON(url,flatten=T))
        if (class(xs)!="try-error") break
        Sys.sleep(sleep_sec)
      }
      # ERROR: frost is not happy with our request, or it is in a bad mood
      if (class(xs)=="try-error") return(NULL)
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
        # data frame as id,lon,lat,elev,stationholder
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
    if (!exists("metaStat")) return(NULL)
    nsou<-length(metaStat$id)
    sourcesstr<-paste("&sources=", paste(metaStat$id,collapse=","),sep="")
    # selection based on the station holder
    if (!is.null(stationholders)) {
      match<-vector(mode="numeric",length=nsou)
      match[]<-NA
      if (stationholders.exclude) {
        for (i in 1:nsou) if (any(sthold[[i]] %in% stationholders)) match[i]<-NA
      } else {
        for (i in 1:nsou) if (any(sthold[[i]] %in% stationholders)) match[i]<-i
      }
      if (!any(!is.na(match))) {
        print("no data available for the stationholder(s) selected")
        return(NULL)
      }
      ix<-which(!is.na(match))
      metaStattmp<-metaStat
      rm(metaStat)
      metaStat<-data.frame(metaStattmp$id[match[ix]],
                           metaStattmp$lon[match[ix]],
                           metaStattmp$lat[match[ix]],
                           metaStattmp$z[match[ix]],
                           metaStattmp$wmoid[match[ix]],
                           stringsAsFactors=F)
      names(metaStat)<-c("id","lon","lat","z","wmoid")
      nsou<-length(metaStat$id)
      stholdtmp<-sthold
      sthold<-list()
      for (i in 1:nsou) sthold[[i]]<-stholdtmp[[match[ix[i]]]]
    }
  # in case no metadata are required
  } else {
    metaSens<-NULL
    stationholders<-NULL
  }
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
             "fields=sourceId,performanceCategory,exposureCategory,",
             "timeOffset,timeResolution",
             elementIdstr,
             timeOffsetstr,
             timeResolutionstr,
             level.valuestr,
             level.levelTypestr,
             datestr,
             sep="")
  for (k in 1:try.again) {
    xs<-try(fromJSON(url,flatten=T))
    if (class(xs)!="try-error") break
    Sys.sleep(sleep_sec)
  }
  # ERROR: frost is not happy with our request, or it is in a bad mood
  if (class(xs)=="try-error") return(NULL)
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
      metaSens<-data.frame(sourcesaux[ix],
                           sensIdaux[ix],
                           xs$data$sourceId[ix],
                           xs$data$performanceCategory[ix],
                           xs$data$exposureCategory[ix],
                           metaStat$lon[match[ix]],
                           metaStat$lat[match[ix]],
                           metaStat$z[match[ix]],
                           stringsAsFactors=F)
      names(metaSens)<-c("source",
                         "sensId",
                         "sourceId",
                         "performanceCategory",
                         "exposureCategory",
                         "lon",
                         "lat",
                         "z")
    } else {
      print(paste("no data available: the two sets of (i) selected stations",
                  "and (ii) available sensors did not match"))
      return(NULL)
    }
  } else {
    print("no data available for the Welement/time selected")
    return(NULL)
  }
  nsouId<-length(metaSens$sensId)
  rm(xs) 
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
  i<-as.numeric(x[2])
  frost_e$value_qcode[i,1]<-ifelse(is.null(x[[1]]$value),
                     NA,as.numeric(x[[1]]$value))
  frost_e$value_qcode[i,2]<-ifelse(is.null(x[[1]]$qualityCode),
                     NA,as.numeric(x[[1]]$qualityCode))
  levaux<-ifelse(is.null(x[[1]]$level),
           NA,x[[1]]$level)
  levTaux<-ifelse(is.null(x[[1]]$levelType),
            "",x[[1]]$levelType)
  elIdaux<-ifelse(is.null(x[[1]]$elementId),
            "",x[[1]]$elementId)   
  tOffaux<-ifelse(is.null(x[[1]]$timeOffset),
            "",x[[1]]$timeOffset)
  tResaux<-ifelse(is.null(x[[1]]$timeResolution),
            "",x[[1]]$timeResolution)
  if (!is.na(levaux) & levTaux!="") {
    aux<-levaux==level.value &
         levTaux==level.levelType
  } else {
    aux<-rep(T,length=length(elementId))
  }
  aux<- aux & elIdaux==elementId      &
              tOffaux==timeOffset     &
              tResaux==timeResolution
  aux.nas<-T
  if (na.rm) aux.nas<-!is.na(frost_e$value_qcode[i,1])
  if ( any(aux) & aux.nas) {
    frost_e$posok[i]<-i
    frost_e$elId[i]<-elIdaux
    frost_e$tOff[i]<-tOffaux
    frost_e$tRes[i]<-tResaux
    if (!is.na(levaux) & levTaux!="") {
      frost_e$lev[i]<-levaux 
      frost_e$levT[i]<-levTaux
    } 
    if (!is.null(oldElementCodes)) {
      j<-which(aux)
      frost_e$oelId[i]<-oldElementCodes[j]
    }
  }
} # END of FUN     
    souIdstep<-65
    sourcesIdstr<-paste("&sources=", paste(metaSens$sourceId,collapse=","),sep="")
    str1<-paste(str0,
               "/observations/v0.jsonld?",
               "fields=elementId,sourceId,value,referenceTime,qualityCode,",
               "timeResolution,timeOffset,level",
               elementIdstr,
               timeOffsetstr,
               timeResolutionstr,
               level.valuestr,
               level.levelTypestr,
               datestr,
               sep="")
    url<-paste(str1,
               sourcesIdstr,
               sep="")
    #NOTE: Frost API has a limit of 2048 characters for the url, however
    # fromJSON refuses to work if the url has more than 1000 characters
    # 1000 is hard-coded in fromJSON
    if (nchar(url)>=1000) {
      for (i in 1:ceiling(nsouId/souIdstep)) {
        i1<-(i-1)*souIdstep+1
        i2<-min(i*souIdstep,nsouId)
        if (i2<i1) break
        sourcesIdstr1<-paste("&sources=", 
                             paste(metaSens$sourceId[i1:i2],
                             collapse=","),sep="")
        url<-paste(str1,
                   sourcesIdstr1,
                   sep="")
        for (k in 1:try.again) {
          xs<-try(fromJSON(url,flatten=T))
          if (class(xs)!="try-error") break
          Sys.sleep(sleep_sec)
        }
        # ERROR: frost is not happy with our request, or it is in a bad mood
        if (class(xs)=="try-error") return(NULL)
        if (xs$totalItemCount==0) next
        # select observations according to na.rm and weather elements
        frost_e<-new.env()
        frost_e$value_qcode<-array(data=NA,dim=c(xs$totalItemCount,2))
        frost_e$posok<-vector(mode="numeric",length=xs$totalItemCount)
        frost_e$elId<-vector(mode="character",length=xs$totalItemCount)
        frost_e$tOff<-vector(mode="character",length=xs$totalItemCount)
        frost_e$tRes<-vector(mode="character",length=xs$totalItemCount)
        frost_e$lev<-vector(mode="numeric",length=xs$totalItemCount)
        frost_e$levT<-vector(mode="character",length=xs$totalItemCount)
        frost_e$oelId<-vector(mode="character",length=xs$totalItemCount)
        frost_e$posok[]<-NA
        frost_e$lev[]<-NA        
        frost_e$levT[]<-""
        frost_e$oelId[]<-""
        devnull<-apply(cbind(xs$data$observations,1:xs$totalItemCount),
                       MARGIN=1,
                       FUN=update_frost_e)
        rm(devnull)
        ix<-which(!is.na(frost_e$posok))
        if (length(ix)==0) next
        op <- options(digits.secs = 3)
        dates<-as.POSIXlt(str2Rdate(xs$data$referenceTime[ix],
                                    format="%Y-%m-%dT%H:%M:%S"))
        datesout<-Rdate2str(dates,formatOUT)
        if (!exists("frost_data")) {
          frost_data<-data.frame(frost_e$elId[ix],
                           xs$data$sourceId[ix],
                           datesout, 
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
                                 xs$data$sourceId[ix],
                                 datesout, 
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
      }
      if (!exists("frost_data")) {
        frost_data<-integer(0)
      } else {
        names(frost_data)<-c("elementId",
                             "sourceId",
                             "date_time",
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
      for (k in 1:try.again) {
        xs<-try(fromJSON(url,flatten=T))
        if (class(xs)!="try-error") break
        Sys.sleep(sleep_sec)
      }
      # ERROR: frost is not happy with our request, or it is in a bad mood
      if (class(xs)=="try-error") return(NULL)
      if (xs$totalItemCount>0) {
        # select observations according to na.rm and weather elements
        frost_e<-new.env()
        frost_e$value_qcode<-array(data=NA,dim=c(xs$totalItemCount,2))
        frost_e$posok<-vector(mode="numeric",length=xs$totalItemCount)
        frost_e$elId<-vector(mode="character",length=xs$totalItemCount)
        frost_e$tOff<-vector(mode="character",length=xs$totalItemCount)
        frost_e$tRes<-vector(mode="character",length=xs$totalItemCount)
        frost_e$lev<-vector(mode="numeric",length=xs$totalItemCount)
        frost_e$levT<-vector(mode="character",length=xs$totalItemCount)
        frost_e$oelId<-vector(mode="character",length=xs$totalItemCount)
        frost_e$posok[]<-NA
        frost_e$lev[]<-NA        
        frost_e$levT[]<-""
        frost_e$oelId[]<-""
        devnull<-apply(cbind(xs$data$observations,1:xs$totalItemCount),
                       MARGIN=1,
                       FUN=update_frost_e)
        rm(devnull)
        ix<-which(!is.na(frost_e$posok))
        if (length(ix)==0) {
          frost_data<-integer(0)
        } else {
          op <- options(digits.secs = 3)
          dates<-as.POSIXlt(str2Rdate(xs$data$referenceTime[ix],
                                      format="%Y-%m-%dT%H:%M:%S"))
          datesout<-Rdate2str(dates,formatOUT)
          frost_data<-data.frame(frost_e$elId[ix],
                           xs$data$sourceId[ix],
                           datesout, 
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
                         "date_time",
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
    }
  # caso of doit.data=F
  } else {
    frost_data<-NULL
  }
  # Normal exit
  return(list(frost_data=frost_data,
              metaSens=metaSens,
              stationholders=sthold))
}
