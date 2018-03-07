#+ get in-situ observations from frost.met.no
`gibson_frost`<-function(client_id=NULL,
                         Welements=NULL,
                         sources="ALL",
                         start_date=NULL,
                         stop_date=NULL,
                         format="%Y-%m-%dT%H:%M",
                         formatOUT="%Y-%m-%dT%H",
                         countries="NO",
                         spatial_extent=c(4,34,54,72),
                         stationholders=NULL,
                         stationholders.exclude=F,
                         na.rm=T)
{
#------------------------------------------------------------------------------
# Each station is identified by a source string SNxx..x (1 to 7 x chars)
# each sensor is identified by the station source string plus an additional id
# so SNxx..x:y (y=0 identifies the first sensor,y=1 thr second,...)
#------------------------------------------------------------------------------
#
  client_id<-"3435de13-091f-4a74-99e8-18c71cef7d97"
  if (is.null(client_id)) {
    print("ERROR you are required to specify a client_id")
    print("see https://frost.met.no/concepts#getting_started")
    return(NULL)
  }
  if (is.null(Welements)) {
    print("ERROR at least one weather element is needed")
    print("see https://frost.met.no/elementtable")
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
  # replace white spaces with %20, so that url works 
  WelementsMod<-gsub(" ","%20",Welements)
  Welementsstr<-paste("&elements=",paste(WelementsMod,collapse=","),sep="")
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
  str1<-paste(str0,
              "/sources/v0.jsonld?types=SensorSystem",
              "&fields=id,geometry,masl,stationholders",
              sep="")
  if (!is.null(spatial_extent)) str1<-paste(str1,polygonstr,sep="")
  # case of all stations are requested
  if (sources=="ALL") {
    # query by country
    if (!is.null(countries)) {
      for (i in 1:length(countries)) {
        url<-paste(str1,"&country=",countries[i],sep="")
        print(url)
        xs<-try(fromJSON(url,flatten=T))
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
          # select only station within the region specified
          if (!is.null(spatial_extent)) {
            ix<-which( xy[,1]>=spatial_extent[1] & 
                       xy[,1]<=spatial_extent[2] &
                       xy[,2]>=spatial_extent[3] &
                       xy[,2]<=spatial_extent[4] )
          } else {
            ix<-1:xs$totalItemCount
          }
          # data frame as id,lon,lat,elev,stationholder
          tmp<-data.frame(xs$data$id[ix],
                          xy[ix,],
                          xs$data$masl[ix],
                          stringsAsFactors=F)
          names(tmp)<-c("id","lon","lat","z")
          # station holders selection over the region of interest
          tmphold<-list()
          devnull<-apply(cbind(xs$data$stationHolders,1:xs$totalItemCount),
                         MARGIN=1,
                         FUN=function(x){ if(x[2]%in%ix) tmphold[[which(ix==x[2])]]<-x[[1]];
                         assign("tmphold",tmphold,envir = .GlobalEnv)})
          rm(devnull)
            #update global data structure given the results for this country
          if (!exists("metaStat")) {
            metaStat<-tmp
            sthold<-tmphold
          } else {
            metaStat<-rbind(metaStat,tmp)
            sthold<-c(sthold,tmphold) 
          }
          rm(tmp,tmphold)
        } # end IF proceed only if we got some data
        rm(xs)
      } #end FOR cycle over countries
    # generic query
    } else {
      url<-str1
      print(url)
      xs<-try(fromJSON(url,flatten=T))
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
        # select only station within the region specified
        if (!is.null(spatial_extent)) {
          ix<-which( xy[,1]>=spatial_extent[1] & 
                     xy[,1]<=spatial_extent[2] &
                     xy[,2]>=spatial_extent[3] &
                     xy[,2]<=spatial_extent[4] )
        } else {
          ix<-1:xs$totalItemCount
        }
        # data frame as id,lon,lat,elev,stationholder
        metaStat<-data.frame(xs$data$id[ix],
                             xy[ix,],
                             xs$data$masl[ix],
                             stringsAsFactors=F)
        names(metaStat)<-c("id","lon","lat","z")
        # station holders selection over the region of interest
        sthold<-list()
        devnull<-apply(cbind(xs$data$stationHolders,1:xs$totalItemCount),
                       MARGIN=1,
                       FUN=function(x){ if(x[2]%in%ix) sthold[[which(ix==x[2])]]<-x[[1]];
                       assign("sthold",sthold,envir = .GlobalEnv)})
        rm(devnull)
      }
      rm(xs)
    } # end IF query for metadata 
  # sources are specified as a vector of characters
  } else {
    sourcesstr<-paste("&sources=",paste(sources,collapse=","),sep="")
    url<-paste(str1,sources,sep="")
    xs<-try(fromJSON(url,flatten=T))
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
      # select only station within the region specified
      if (!is.null(spatial_extent)) {
        ix<-which( xy[,1]>=spatial_extent[1] & 
                   xy[,1]<=spatial_extent[2] &
                   xy[,2]>=spatial_extent[3] &
                   xy[,2]<=spatial_extent[4] )
      } else {
        ix<-1:xs$totalItemCount
      }
      # data frame as id,lon,lat,elev,stationholder
      metaStat<-data.frame(xs$data$id[ix],
                           xy[ix,],
                           xs$data$masl[ix],
                           stringsAsFactors=F)
      names(metaStat)<-c("id","lon","lat","z")
      # station holders selection over the region of interest
      sthold<-list()
      devnull<-apply(cbind(xs$data$stationHolders,1:xs$totalItemCount),
                     MARGIN=1,
                     FUN=function(x){ if(x[2]%in%ix) sthold[[which(ix==x[2])]]<-x[[1]];
                     assign("sthold",sthold,envir = .GlobalEnv)})
      rm(devnull)
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
                         stringsAsFactors=F)
    names(metaStat)<-c("id","lon","lat","z")
    nsou<-length(metaStat$id)
    stholdtmp<-sthold
    sthold<-list()
    for (i in 1:nsou) sthold[[i]]<-stholdtmp[[match[ix[i]]]]
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
             Welementsstr,
             datestr,
             sep="")
  xs<-try(fromJSON(url,flatten=T))
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
  souIdstep<-75
  sourcesIdstr<-paste("&sources=", paste(metaSens$sourceId,collapse=","),sep="")
  str1<-paste(str0,
             "/observations/v0.jsonld?",
             "fields=elementId,sourceId,value,referenceTime,qualityCode",
             Welementsstr,
             datestr,
             sep="")
  url<-paste(str1,
             sourcesIdstr,
             sep="")
  # more than 1000 characters and fromJSON refuses to work, that number is 
  # hard-coded in the function
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
      xs<-try(fromJSON(url,flatten=T))
      # ERROR: frost is not happy with our request, or it is in a bad mood
      if (class(xs)=="try-error") return(NULL)
      if (xs$totalItemCount>0) next
      value_qcode<-array(data=NA,dim=c(xs$totalItemCount,2))
      elId<-vector(mode="character",length=xs$totalItemCount)
      devnull<-apply(cbind(xs$data$observations,1:xs$totalItemCount),
                     MARGIN=1,
                     FUN=function(x){
         value_qcode[as.numeric(x[2]),]<-c(as.numeric(x[[1]]$value),
                                           as.numeric(x[[1]]$qualityCode));
         assign("value_qcode",value_qcode,envir = .GlobalEnv);
         elId[as.numeric(x[2])]<-x[[1]]$elementId;
         assign("elId",elId,envir = .GlobalEnv) }
                     ) 
      rm(devnull)
      ix<-which(!is.na(as.numeric(value_qcode[,1])))
      if (length(ix)==0) next
      op <- options(digits.secs = 3)
      dates<-as.POSIXlt(str2Rdate(xs$data$referenceTime[ix],
                                  format="%Y-%m-%dT%H:%M:%S"))
      datesout<-Rdate2str(dates,formatOUT)
      if (!exists("data")) {
        data<-data.frame(elId[ix],
                         xs$data$sourceId[ix],
                         datesout, 
                         value_qcode[ix,],
                           stringsAsFactors=F)
        names(data)<-c("Welement","sourceId","date_time","value","qcode")
      } else {
        data<-cbind(data,
                    data.frame(elId[ix],
                               xs$data$sourceId[ix],
                               datesout, 
                               value_qcode[ix,],
                                stringsAsFactors=F) )
      }
    }
    if (!exists("data")) data<-integer(0)
  # all the data retrieved in one shot
  } else {
    xs<-try(fromJSON(url,flatten=T))
    # ERROR: frost is not happy with our request, or it is in a bad mood
    if (class(xs)=="try-error") return(NULL)
    if (xs$totalItemCount>0) {
      value_qcode<-array(data=NA,dim=c(xs$totalItemCount,2))
      elId<-vector(mode="character",length=xs$totalItemCount)
      devnull<-apply(cbind(xs$data$observations,1:xs$totalItemCount),
                     MARGIN=1,
                     FUN=function(x){
         value_qcode[as.numeric(x[2]),]<-c(as.numeric(x[[1]]$value),
                                           as.numeric(x[[1]]$qualityCode));
         assign("value_qcode",value_qcode,envir = .GlobalEnv);
         elId[as.numeric(x[2])]<-x[[1]]$elementId;
         assign("elId",elId,envir = .GlobalEnv) }
                     ) 
      rm(devnull)
      ix<-which(!is.na(as.numeric(value_qcode[,1])))
      if (length(ix)==0) {
        data<-integer(0)
      } else {
        op <- options(digits.secs = 3)
        dates<-as.POSIXlt(str2Rdate(xs$data$referenceTime[ix],
                                    format="%Y-%m-%dT%H:%M:%S"))
        datesout<-Rdate2str(dates,formatOUT)
        data<-data.frame(elId[ix],
                         xs$data$sourceId[ix],
                         datesout, 
                         value_qcode[ix,],
                           stringsAsFactors=F)
        names(data)<-c("Welement","sourceId","date_time","value","qcode")
    } else {
      data<-integer(0)
    }
    rm(xs)
  }
  # Normal exit
  return(list(data=data,
              metaSens=metaSens))
}
