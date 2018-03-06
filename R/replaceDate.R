# + replace elements of a string with date-time elements
`replaceDate`<-function(string=NULL,
                        date.str=NULL,
                        format="%Y-%m-%d %H:%M:%S") {
#------------------------------------------------------------------------------
  if (is.null(string) | is.null(date.str)) return(NULL)
  Rdate<-as.POSIXlt(str2Rdate(date.str,format=format))
  yyyy<-Rdate$year+1900
  mm<-formatC(Rdate$mon+1,width=2,flag="0")
  dd<-formatC(Rdate$mday,width=2,flag="0")
  hh<-formatC(Rdate$hour,width=2,flag="0")
  out<-gsub("yyyy",yyyy,string)
  out<-gsub("mm",formatC(mm,width=2,flag="0"),out)
  out<-gsub("dd",formatC(dd,width=2,flag="0"),out)
  out<-gsub("hh",formatC(hh,width=2,flag="0"),out)
  out
}

