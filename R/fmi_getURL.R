#+ get the url for the query to FMI open data API
fmi_getURL<-function(apiKey=NULL,
                     abbrv=NULL, #"minute" "day"
                     starttime=NULL,
                     endtime=NULL,
                     format="%Y-%m-%dT%H:%M",
                     parameters=NULL,
                     bbox=c(19.09,59.3,31.59,70.13)) {
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
  if (is.null(apiKey)|is.null(abbrv)) return(NULL)
  bbox_str<-as.character(bbox[1])
  for (i in 2:length(bbox)) bbox_str<-paste0(bbox_str,",",bbox[i])
  if (!is.null(starttime) & !is.null(endtime)) {
    formatFMI<-"%Y-%m-%dT%H:%M:%SZ"
    Rdate_start<-as.POSIXlt(str2Rdate(starttime,format=format))
    Rdate_end<-as.POSIXlt(str2Rdate(endtime,format=format))
    starttimeMod<-Rdate2str(Rdate_start,formatFMI)
    endtimeMod<-Rdate2str(Rdate_end,formatFMI)
    timestr<-paste0("&starttime=",starttimeMod,
                    "&endtime=",endtimeMod)
  } else {
    timestr<-""
  }
  if (!is.null(parameters)) {
    par_str<-paste0("&parameters=",parameters[1])
    for (i in 2:length(parameters)) 
      par_str<-paste0(par_str,",",parameters[i])
  } else {
    par_str<-""
  }
  if (abbrv=="minute") {
    qid_str<-"fmi::observations::weather::multipointcoverage" 
  } else if (abbrv=="day") {
    qid_str<-"fmi::observations::weather::daily::multipointcoverage" 
  } else {
    print("fmi_getURL.R ERROR: abbrv must be either \"minute\" or \"day\"")
    qid_str<-"" 
  }
  url<-paste0("http://data.fmi.fi/fmi-apikey/",apiKey,
              "/wfs?request=GetFeature",
              "&storedquery_id=",qid_str,
              "&bbox=",bbox_str,
              par_str,
              timestr)
  url
}

