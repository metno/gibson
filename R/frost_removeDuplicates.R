#+ remove possible duplicates in the frost output
`frost_removeDuplicates`<-function(oldElementCode=NULL,
                                   frost_data=NULL) {
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
  # in case the element does not have duplicates
  if (oldElementCode %in% c("RR_1","TA","TAMRR","TAM")) {
    ixNoDup<-1:length(frost_data$value)
  # case of RR
  }  else if (oldElementCode=="RR") {
    referenceTime<-as.POSIXlt(str2Rdate(frost_data$referenceTime,
                                        format="%Y-%m-%dT%H:%M:%S"))
    day<-Rdate2str(referenceTime,"%Y-%m-%d")
    hour<-Rdate2str(referenceTime,"%H")
#    dupflag<-duplicated(data.frame(frost_data$sourceId,
#                                   day,
#                                   frost_data$value,
#                                   frost_data$qcode))
    # NOTE: for the same sourceId/day/value we may have two
    #       different qcodes. I've seen it. current solution:
    #       pick one record at random
    dupflag<-duplicated(data.frame(frost_data$sourceId,
                                   day,
                                   frost_data$value,
                                   stringsAsFactors=F))
    ixNoDup<-which(!dupflag)
  }
  ixNoDup
}
