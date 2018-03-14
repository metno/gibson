#+
`frost_translate_oldElementCodes`<-function(oldElementCodes=NULL) {
#------------------------------------------------------------------------------
# Documentation: see help(frost_translate_oldElementCodes) on R or 
# gibson/man/frost_translate_oldElementCodes.Rd
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
  if (is.null(oldElementCodes)) return(NULL)
  nEl<-length(oldElementCodes)
  ElementCodes<-data.frame(
    elementId=vector(mode="character",length=nEl),
    timeOffset=vector(mode="character",length=nEl),
    timeResolution=vector(mode="character",length=nEl),
    level.value=vector(mode="numeric",length=nEl),
    level.levelType=vector(mode="character",length=nEl),
  stringsAsFactors=F) 
  for (i in 1:nEl) {
    switch(oldElementCodes[i],
     "RR_1" = {   
       ElementCodes$elementId[i]="sum(precipitation_amount PT1H)"
       ElementCodes$timeOffset[i]="PT00H"
       ElementCodes$timeResolution[i]="PT1H"
       ElementCodes$level.value[i]=NA
       ElementCodes$level.levelType[i]=""
              },
       "TA" = {
       ElementCodes$elementId[i]="air_temperature"
       ElementCodes$timeOffset[i]="PT00H"
       ElementCodes$timeResolution[i]="PT1H"
       ElementCodes$level.value[i]=2
       ElementCodes$level.levelType[i]="height_above_ground"
              },
      "TAM" = {  
       ElementCodes$elementId[i]="mean(air_temperature P1D)"
       ElementCodes$timeOffset[i]="PT00H"
       ElementCodes$timeResolution[i]="P1D"
       ElementCodes$level.value[i]=2
       ElementCodes$level.levelType[i]="height_above_ground"
              },
    "TAMRR" = {  
       ElementCodes$elementId[i]="mean(air_temperature P1D)"
       ElementCodes$timeOffset[i]="PT06H"
       ElementCodes$timeResolution[i]="P1D"
       ElementCodes$level.value[i]=2
       ElementCodes$level.levelType[i]="height_above_ground"
              },
       "RR" = {  
       ElementCodes$elementId[i]="sum(precipitation_amount P1D)"
       ElementCodes$timeOffset[i]="PT06H"
       ElementCodes$timeResolution[i]="P1D"
       ElementCodes$level.value[i]=NA
       ElementCodes$level.levelType[i]=""
              },
       "FF" = {  
       ElementCodes$elementId[i]="wind_speed"
       ElementCodes$timeOffset[i]="PT00H"
       ElementCodes$timeResolution[i]="PT1H"
       ElementCodes$level.value[i]=10
       ElementCodes$level.levelType[i]="height_above_ground"
              },
       "PR" = {  
       ElementCodes$elementId[i]="air_pressure_at_sea_level"
       ElementCodes$timeOffset[i]="PT00H"
       ElementCodes$timeResolution[i]="PT1H"
       ElementCodes$level.value[i]=NA
       ElementCodes$level.levelType[i]=""
              },
       "DG" = {  
       ElementCodes$elementId[i]="wind_from_direction_of_gust"
       ElementCodes$timeOffset[i]="PT00H"
       ElementCodes$timeResolution[i]="PT1H"
       ElementCodes$level.value[i]=10
       ElementCodes$level.levelType[i]="height_above_ground"
              },
       "DD" = {  
       ElementCodes$elementId[i]="wind_from_direction"
       ElementCodes$timeOffset[i]="PT00H"
       ElementCodes$timeResolution[i]="PT1H"
       ElementCodes$level.value[i]=10
       ElementCodes$level.levelType[i]="height_above_ground"
              },
       "FG" = {  
       ElementCodes$elementId[i]="wind_speed_of_gust"
       ElementCodes$timeOffset[i]="PT00H"
       ElementCodes$timeResolution[i]="PT1H"
       ElementCodes$level.value[i]=10
       ElementCodes$level.levelType[i]="height_above_ground"
              },
       "TAN" = {  
       ElementCodes$elementId[i]="min(air_temperature P1D)"
       ElementCodes$timeOffset[i]="PT18H"
       ElementCodes$timeResolution[i]="P1D"
       ElementCodes$level.value[i]=2
       ElementCodes$level.levelType[i]="height_above_ground"
              },
       "TAX" = {  
       ElementCodes$elementId[i]="max(air_temperature P1D)"
       ElementCodes$timeOffset[i]="PT18H"
       ElementCodes$timeResolution[i]="P1D"
       ElementCodes$level.value[i]=2
       ElementCodes$level.levelType[i]="height_above_ground"
              },
       "UU" = {  
       ElementCodes$elementId[i]="relative_humidity"
       ElementCodes$timeOffset[i]="PT00H"
       ElementCodes$timeResolution[i]="PT1H"
       ElementCodes$level.value[i]=NA
       ElementCodes$level.levelType[i]=""
              },
       # default: element is not defined
       {
       ElementCodes$elementId[i]=""
       ElementCodes$timeOffset[i]=""
       ElementCodes$timeResolution[i]=""
       ElementCodes$level.value[i]=NA
       ElementCodes$level.levelType[i]=""
       }
    ) # END of switch
  } # END FOR
  ElementCodes
}
  

