#+
`fmi_opendata_translate_oldElementCodes`<-function(oldElementCodes=NULL,
                                                   fmi_parameterId=NULL) {
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
  if (!is.null(oldElementCodes)) {
    nEl<-length(oldElementCodes)
    parameterId<-vector(mode="character",length=nEl)
    for (i in 1:nEl) {
      switch(oldElementCodes[i],
       "RR_1" = {  
         parameterId[i]<-"r_1h"
                },
         "TA" = {
         parameterId[i]<-"t2m"
                },
        "TAM" = {  
         parameterId[i]<-"tday"
                },
      "TAMRR" = {  
         parameterId[i]<-"tday"
                },
         "RR" = {  
         parameterId[i]<-"rrday"
                },
         "FF" = {  
         parameterId[i]<-"ws_10min"
                },
         "PR" = {  
         parameterId[i]<-"p_sea"
                },
         "DG" = {  
         parameterId[i]<-NA
                },
         "DD" = {  
         parameterId[i]<-"wd_10min"
                },
         "FG" = {  
         parameterId[i]<-"wg_10min"
                },
         "TAN" = {  
         parameterId[i]<-"tmin"
                },
         "TAX" = {  
         parameterId[i]<-"tmax"
                },
         "UU" = {  
         parameterId[i]<-"rh"
                },
         # default: element is not defined
         {
         parameterId[i]<-NA
         }
      ) # END of switch
    } # END FOR
  } else if (!is.null(fmi_parameterId)) {
    nEl<-length(fmi_parameterId)
    parameterId<-vector(mode="character",length=nEl)
    for (i in 1:nEl) {
      switch(fmi_parameterId[i],
        "r_1h" = {  
         parameterId[i]<-"RR_1"
                },
         "t2m" = {
         parameterId[i]<-"TA"
                },
        "tday" = {  
         parameterId[i]<-"TAM"
                },
       "rrday" = {  
         parameterId[i]<-"RR"
                },
    "ws_10min" = {  
         parameterId[i]<-"FF"
                },
       "p_sea" = {  
         parameterId[i]<-"PR"
                },
    "wd_10min" = {  
         parameterId[i]<-"DD"
                },
    "wg_10min" = {  
         parameterId[i]<-"FG"
                },
        "tmin" = {  
         parameterId[i]<-"TAN"
                },
        "tmax" = {  
         parameterId[i]<-"TAX"
                },
          "rh" = {  
         parameterId[i]<-"UU"
                },
         # default: element is not defined
         {
         parameterId[i]<-NA
         }
      ) # END of switch
    } # END FOR
  } else {
    return(NULL)
  }
  parameterId
}
  

