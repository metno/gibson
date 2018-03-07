`Rdate2str` <-
function(date,format="%Y-%m-%d %H:%M:%S") {
# ===========================================================================
# converts a R (POSIXt,POSIXct) date object into a string
# date is one or a vector of R date-time objects
# format specifies the desired character format analogous to str2Rdate
# Output is one or a vector of characters
     format(date,format=format,tz="GMT")
}

