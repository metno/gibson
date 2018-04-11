#+ download the FMI station list
`fmi_station_list`<-function(
  url="http://en.ilmatieteenlaitos.fi/observation-stations") {
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
  html<-read_html(url)
  table<-strsplit(xml_text(xml_find_all(html,"..//table")),"\n")
  nrow<-length(table[[1]])
  headf<-F
  head<-vector()
  j<-0
  k<-0
  el_jump<-F
  jump<-T
  for (i in 1:nrow) {
    clean<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$","",table[[1]][i], perl=TRUE)
#    print(clean)
    if (clean=="Name") {
      headf<-T
      jump<-F
    }
    if (jump) next
    if (headf) {
      j<-j+1
      head[j]<-clean
    }
    if (headf & clean=="Started") {
      headf<-F
      stn<-array(data=NA,dim=c(nrow,j))
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
      j<-0
      k<-k+1
      next    
    }
    if (headf) next
    if (el_jump) {
      el_jump<-F
      next 
    }
    j<-j+1
    if (head[j]=="Name") fmi.name[k]<-ifelse(clean=="",NA,clean)
    if (head[j]=="FMISID") fmi.fmisid[k]<-clean
    if (head[j]=="WMO") fmi.wmo[k]<-ifelse(clean=="",NA,clean)
    if (head[j]=="Elevation") fmi.elevation[k]<-as.numeric(clean)
    if (head[j]=="Groups") fmi.group[k]<-ifelse(clean=="",NA,clean)
#    if (head[j]=="Started") stn[k,j]<-as.numeric(clean)
#    stn[k,j]<-clean
    if (head[j]=="Started") {
      j<-0
      k<-k+1
      next
    }
    if (head[j]=="Elevation") el_jump<-T
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


