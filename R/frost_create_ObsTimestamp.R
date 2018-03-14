`frost_create_ObsTimestamp`<-function(frost_data=NULL){
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
  referenceTime<-as.POSIXlt(str2Rdate(frost_data$referenceTime,
                                      format="%Y-%m-%dT%H:%M:%S"),tzone="UTC")
  timestamp<-structure( rep(NA_real_,length(referenceTime)),
                        class=c("POSIXct", "POSIXt") )
  # hourly elements has timestamp equal to referenceTime
  ix<-which(frost_data$timeResolution=="PT1H")
  if (length(ix)>0) timestamp[ix]<-referenceTime[ix]
  # daily
  ix<-which(frost_data$timeResolution=="P1D")
  if (length(ix)>0) {
    day<-Rdate2str(referenceTime[ix],"%Y-%m-%d")
    referenceTime[ix]<-as.POSIXlt(
                        str2Rdate(paste(day,"-",rep("00",length(day)),sep=""),
                                        format="%Y-%m-%d-%H"),tzone="UTC")
    timeOffset<-as.numeric(substr(frost_data$timeOffset[ix],3,4))
    timestamp[ix]<-referenceTime[ix]+as.difftime(timeOffset, unit="hours")
    rm(day,timeOffset)
  }
  #...
  # return
  timestamp
}
