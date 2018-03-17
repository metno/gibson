#+
`smhi_opendata_translate_oldElementCodes`<-function(oldElementCodes=NULL) {
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
# to see the parameter Ids go to
# https://opendata-download-metobs.smhi.se/api/version/1.0.json
  if (is.null(oldElementCodes)) return(NULL)
  nEl<-length(oldElementCodes)
  parameterId<-vector(mode="numeric",length=nEl)
  for (i in 1:nEl) {
    switch(oldElementCodes[i],
     "RR_1" = {  
       parameterId[i]<-7
              },
       "TA" = {
       parameterId[i]<-1
              },
      "TAM" = {  
       parameterId[i]<-NA
              },
    "TAMRR" = {  
       parameterId[i]<-NA
              },
       "RR" = {  
       parameterId[i]<-5
              },
       "FF" = {  
       parameterId[i]<-4
              },
       "PR" = {  
       parameterId[i]<-9
              },
       "DG" = {  
       parameterId[i]<-NA
              },
       "DD" = {  
       parameterId[i]<-3
              },
       "FG" = {  
       parameterId[i]<-21
              },
       "TAN" = {  
       parameterId[i]<-19
              },
       "TAX" = {  
       parameterId[i]<-20
              },
       "UU" = {  
       parameterId[i]<-6
              },
       # default: element is not defined
       {
       parameterId[i]<-NA
       }
    ) # END of switch
  } # END FOR
  parameterId
}
  

