#+ download the FMI station list
`fmi_station_list`<-function(
  url_oper="http://en.ilmatieteenlaitos.fi/observation-stations",
  url_hist="http://en.ilmatieteenlaitos.fi/observation-stations?p_p_id=stationlistingportlet_WAR_fmiwwwweatherportlets&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=column-4&p_p_col_count=1&_stationlistingportlet_WAR_fmiwwwweatherportlets_stationGroup=ENDED#station-listing",
  try.again=3,
  sleep_sec=5) {
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
  suppressPackageStartupMessages(require(xml2))
# no scientific notation so to get correct unix timestamp
  options(scipen=999)
#
  for (k in 1:try.again) {
    html_oper<-try(read_html(url_oper))
    if (class(html_oper)[1]!="try-error") break
    Sys.sleep(sleep_sec)
  }
  if (class(html_oper)[1]=="try-error") return(NULL)
  for (k in 1:try.again) {
    html_hist<-try(read_html(url_hist))
    if (class(html_hist)[1]!="try-error") break
    Sys.sleep(sleep_sec)
  }
  if (class(html_hist)[1]=="try-error") return(NULL)
  #  
  table_oper<-strsplit(xml_text(xml_find_all(html_oper,"..//table")),"\n")
  nrow_oper<-length(table_oper[[1]])
  table_hist<-strsplit(xml_text(xml_find_all(html_hist,"..//table")),"\n")
  nrow_hist<-length(table_hist[[1]])
  nrow<-nrow_oper+nrow_hist
  fmi.elevation<-vector(mode="numeric",length=nrow)
  fmi.name<-vector(mode="character",length=nrow)
  fmi.wmo<-vector(mode="character",length=nrow)
  fmi.fmisid<-vector(mode="character",length=nrow)
  fmi.group<-vector(mode="character",length=nrow)
  fmi.elevation[]<-NA
  fmi.name[]<-NA
  fmi.wmo[]<-NA
  fmi.fmisid[]<-NA
  fmi.group[]<-NA
  head_flag<-F
  head<-vector()
  # j conts the columns
  j<-0
  # k counts the rows
  k<-0
  el_jump<-F
  jump<-T
  for (f in 1:2) {
    if (f==1) {
      table<-table_oper
      nrow<-nrow_oper
      end_header_string<-"Started"
    } else if (f==2) {
      table<-table_hist
      nrow<-nrow_hist
      end_header_string<-"Ended"
      j<-0
      el_jump<-F
      jump<-T
    }
    cleanv<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",as.vector(table[[1]]), perl=TRUE)
    for (i in 1:nrow) {
#      clean<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",table[[1]][i], perl=TRUE)
#      print(clean)
      # jump until the "Name" row
      if (cleanv[i]!="Name" & jump) next
      jump<-F
      # -- read the header
      if (cleanv[i]=="Name") head_flag<-T
      if (head_flag) {
        j<-j+1
        head[j]<-cleanv[i]
        # just read the last column of the header
        if (head[j]==end_header_string) {
          head_flag<-F
          ncol<-j
          j<-0
          # prepare for the next data-row to be stored
          if (k==0) k<-k+1
        }
        next
      } # -- end: read the header
      # -- read the metadata 
      # elevation may appear twice, jump the second occurrence
      if (el_jump) {
        el_jump<-F
        next 
      }
      j<-j+1
      if (head[j]=="Name") {
        fmi.name[k]<-ifelse(cleanv[i]=="",NA,cleanv[i])
      } else if (head[j]=="FMISID") {
        fmi.fmisid[k]<-cleanv[i]
      } else if (head[j]=="WMO") {
        fmi.wmo[k]<-ifelse(cleanv[i]=="",NA,cleanv[i])
      } else if (head[j]=="Elevation") {
        fmi.elevation[k]<-suppressWarnings(as.numeric(cleanv[i]))
        el_next<-suppressWarnings(as.numeric(cleanv[(i+1)]))
        if (!is.na(fmi.elevation[k]) & 
            !is.na(el_next)) {
          if (fmi.elevation[k]==el_next) el_jump<-T
        }
      } else if (head[j]=="Groups") {
        fmi.group[k]<-ifelse(cleanv[i]=="",NA,cleanv[i])
      } else if (head[j]==end_header_string) {
        j<-0
        k<-k+1
        next
      }
    }
  }
  nfmi<-length(which(!is.na(fmi.name)))
  meta.stnl<-data.frame(name=fmi.name[1:nfmi],
                        fmisid=fmi.fmisid[1:nfmi],
                        wmo=fmi.wmo[1:nfmi],
                        elevation=fmi.elevation[1:nfmi],
                        group=fmi.group[1:nfmi],
                        stringsAsFactors=F)
  meta.stnl
}


