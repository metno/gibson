#+ get in-situ observations from frost.met.no
`gibson_frost`<-function(client_id="3435de13-091f-4a74-99e8-18c71cef7d97",
                         Welements=NULL,
                         sources="ALL",
                         start_date=NULL,
                         stop_date=NULL,
                         format="%Y-%m-%dT%H:%M",
                         countries="NO",
                         spatial_extent=c(4,34,54,72),
                         na.rm=T)
{
#------------------------------------------------------------------------------
#==============================================================================
  str0<-paste("https://",client_id,"@frost.met.no",sep="")
  if (!is.null(spatial_extent))
    polygonstr<-paste("&geometry=POLYGON((",
                      spatial_extent[1]," ",spatial_extent[3],",",
                      spatial_extent[1]," ",spatial_extent[4],",",
                      spatial_extent[2]," ",spatial_extent[4],",",
                      spatial_extent[2]," ",spatial_extent[3],",",
                      spatial_extent[1]," ",spatial_extent[3],"))",
                      sep="")
  # patch: there is a problem while passing polygonstr to fromJSON
  polygonstr<-""
#------------------------------------------------------------------------------
# >> METADATA <<
  # case of all stations are requested
  if (sources=="ALL") {
    str1<-paste(str0,
                "/sources/v0.jsonld?types=SensorSystem","
                &fields=id,geometry,masl,stationholders",
               sep="")
    if (!is.null(countries)) {
      for (i in 1:length(countries)) {
        url<-paste(str1,"&country=",countries[i],sep="")
        if (!is.null(spatial_extent)) url<-paste(url,polygonstr,sep="")
        print(url)
        xs<-fromJSON(url,flatten=T)
        # ERROR: frost is not happy with our request, or it is in a bad mood
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
          tmp<-data.frame(xs$data$id[ix],
                          xy[ix,],
                          xs$data$masl[ix])
          names(tmp)<-c("id","lon","lat","z")
          # station holders selection over the region of interest
          tmphold<-list()
          devnull<-apply(cbind(xs$data$stationHolders,1:xs$totalItemCount),
                         MARGIN=1,
                         FUN=function(x){ if(x[2]%in%ix) tmphold[[which(ix==x[2])]]<-x[[1]];
                         assign("tmphold",tmphold,envir = .GlobalEnv)})
          rm(devnull)
            #update global data structure given the results for this country
          if (!exists("meta")) {
            meta<-tmp
            sthold<-tmphold
          } else {
            meta<-rbind(meta,tmp)
            sthold<-c(sthold,tmphold) 
          }
          rm(tmp,tmphold)
        } # end IF proceed only if we got some data
      } #end FOR cycle over countries
    } # end IF not null countries
  } # end IF sources==ALL
#------------------------------------------------------------------------------
# DATA 

#------------------------------------------------------------------------------
# QUALITY FLAGS
}

rm(list=ls())
library("httr")

# Autentisering
client_id  <- "ca076a66-7ea1-4ca2-8eaf-f091899a9eba:f8413549-8f24-4ce1-8154-e5a2c85e553c"

# Parametere som skal hentes ut
Welements <- c("wind_speed", "wind_from_direction")

# Stasjon, her kan du godt oppgi flere
sources <-  c("SN4780")

# Tidsrom data skal hentes for
start_date <- "2015-01-01T00:00"
stop_date  <- "2015-01-03T00:00"

# URL for uthenting settes sammen
url <- paste("https://", client_id, "@data.met.no/observations/v0.jsonld?",
             "sources=", paste(sources,collapse=",") ,
             "&referencetime=", start_date, "/", stop_date,
             "&elements=", paste(Welements,collapse=","),
             sep = "", collapse= "")

# Henter ut data via API 
xs <- scan(url, what="")

# Henter ut posisjoner for tid, vindstyrke, vindretning, og stasjonsnummer fra json melding
k  <- grep("referenceTime", xs)
e <- grep("^wind_speed$", xs) # Denne sørger for at kun eksakte strengen
d <- grep("^wind_from_direction$", xs)
s <- grep("sourceId", xs)

# Legger dette over i en tabell   
wsTable <- data.frame(Stnr=xs[s+2], time=xs[k+2],
                      WD=as.numeric(gsub(",", "", xs[d+4])), WS=as.numeric(gsub(",", "", xs[e+4])))

# Tabellen finpusses litt
datetime <- strptime(x = as.character(wsTable$time), format = "%Y-%m-%dT%H:%M:%S", tz="UTC")
wsTable$time <- datetime
