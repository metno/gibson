#+ Create a time sequence having daily/hourly timestep
createTimeSeq<-function(start_date="2015.01.01.01",
                        stop_date="2015.12.31.23",
                        format="%Y.%m.%d.%H",
                        time_step=1,
                        unit="hours",
                        season=NULL,
                        hourOFday.sel=NULL,
                        dayOFmonth.sel=NULL,
                        N.prev=NULL,
                        N.succ=NULL,
                        RdateOnlyOut=F,
                        verbose=F) {
#==============================================================================
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
#==============================================================================
  # set parameters
  if (!is.null(stop_date))
    if  (is.na(stop_date)) stop_date<-NULL
  if (!is.null(season))
    if  (any(is.na(season))) season<-NULL
  if (!is.null(hourOFday.sel))
    if  (any(is.na(hourOFday.sel))) hourOFday.sel<-NULL
  if (!is.null(dayOFmonth.sel))
    if  (any(is.na(dayOFmonth.sel))) dayOFmonth.sel<-NULL
  if (!is.null(N.prev))
    if  (is.na(N.prev)) N.prev<-NULL
  if (!is.null(N.succ))
    if  (is.na(N.succ)) N.succ<-NULL
  #
  mon.s<-0:11
  if (!is.null(season)) {
    mon.s<-integer(0)
    if (any(season=="MAM")) mon.s<-c(mon.s,2,3,4)
    if (any(season=="JJA")) mon.s<-c(mon.s,5,6,7)
    if (any(season=="SON")) mon.s<-c(mon.s,8,9,10)
    if (any(season=="DJF")) mon.s<-c(mon.s,11,0,1)
  }
  hour.s<-0:23
  if (!is.null(hourOFday.sel)) hour.s<-hourOFday.sel
  day.s<-1:31
  if (!is.null(dayOFmonth.sel)) day.s<-dayOFmonth.sel
  # elaboration
  Rstart_date<-as.POSIXlt(str2Rdate(start_date,format=format))
  bystr<-paste(time_step," ",unit,sep="")
  if (is.null(stop_date)) {
    Rstop_date<-Rstart_date+as.difftime(N.succ*time_step, unit=unit)
    Rstart_date<-Rstart_date-as.difftime(N.prev*time_step, unit=unit)
  } else {
    Rstop_date<-as.POSIXlt(str2Rdate(stop_date,format=format))
  }
  tseq<-as.POSIXlt(seq(Rstart_date,Rstop_date,by=bystr),"UTC")
  # 
  ix<-which( (tseq$mon %in% mon.s)  & 
             (tseq$mday %in% day.s) & 
             (tseq$hour %in% hour.s) )
  if (length(ix)==0) return(integer(0))
  if (RdateOnlyOut) return(tseq[ix])
  yyyymm.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                  formatC(tseq$mon[ix]+1,width=2,flag="0"),sep="")
  yyyymmddhh.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                      formatC(tseq$mon[ix]+1,width=2,flag="0"),
                      formatC(tseq$mday[ix],width=2,flag="0"),
                      formatC(tseq$hour[ix],width=2,flag="0"),sep="")
  yyyymmdd.v<-paste(formatC(tseq$year[ix]+1900,width=4,flag="0"),
                    formatC(tseq$mon[ix]+1,width=2,flag="0"),
                    formatC(tseq$mday[ix],width=2,flag="0"),sep="")
  nt<-length(yyyymmddhh.v)
  return(list(n=nt,
              yyyymm=yyyymm.v,
              yyyymmdd=yyyymmdd.v,
              yyyymmddhh=yyyymmddhh.v,
              yyyy=formatC(tseq$year[ix]+1900,width=4,flag="0"),
              mm=formatC(tseq$mon[ix]+1,width=2,flag="0"),
              dd=formatC(tseq$mday[ix],width=2,flag="0"),
              hh=formatC(tseq$hour[ix],width=2,flag="0")))
}
