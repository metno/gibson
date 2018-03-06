# + get netatmo data and metadata
`gibson_netatmo`<-function(date.str=NULL,
                           format=NULL,
                           path="/lustre/storeB/users/thomasn/ecflowdata/products/netatmo",
                           path.date="yyyy/mm/dd",
                           fbname="netatmo_",
                           fbname.date="yyyymmddThhZ",
                           fext="txt",
                           fsep=";",
                           selout=c("lat","lon","elev"),
                           na=-999,
                           na.rm=T) {
#==============================================================================
  # construct full filename
  ffin<-file.path(path,
                  replaceDate(path.date,date.str,format),
                  paste(fbname,
                        replaceDate(fbname.date,date.str,format),".",
                        fext,sep="")
                  )
  if (!file.exists(ffin)) {
    print("ERROR file not found")
    print(ffin)
    return(NULL)
  }
  # read data
  data<-read.table(file=ffin,
                   header=T,
                   sep=fsep,
                   stringsAsFactors=F,
                   strip.white=T)
  # select output
  varidx<-match(selout,names(data))
  if (any(is.na(varidx))) {
    print("ERROR in the specification of the output variable names")
    print("requested fields")
    print(selout)
    print("available fields")
    print(names(data))
    quit(status=1)
  }
  if (na.rm) {
    ix<-which(apply(data[,varidx],MARGIN=1,FUN=function(x)(!any(x==na))))
  } else {
    ix<-1:length(data[,1])
  }
  if (length(ix)==0) {
    dataout<-NULL
  } else {
    dataout<-data.frame(data[ix,varidx])
    names(dataout)<-selout
  }
  dataout
}

