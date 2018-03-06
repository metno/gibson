`str2Rdate` <-
function(ts,format="%Y-%m-%d %H:%M:%S") {
# ===========================================================================
# converts a string into an R (POSIXt,POSIXct) date object
# date objects can be used with arithmetic operations +/-
# ts is a character or a vector of characters with date information
# in the format specified in format
# Output is a date object

     # the lengthy bunch of testing is necessary because strptime needs
     # explicit specifications for month and day, otherwise it returns NA.
     # this extension allows inputs of the format "%Y-%m" in which case the
     # the first day of the month is taken as a reference.

     #Êcheck if year/month/day is specified
     ysp <- length(c(grep("%Y",format,fixed=TRUE),
                     grep("%y",format,fixed=TRUE)))
     msp <- length(c(grep("%m",format,fixed=TRUE),
                     grep("%b",format,fixed=TRUE),
                     grep("%B",format,fixed=TRUE)))
     jsp <- length(c(grep("%j",format,fixed=TRUE)))
     dsp <- length(c(grep("%d",format,fixed=TRUE)))
     if (ysp > 1) { stop("ERROR: Multiple specification of year in 
                         date format.") }
     if (ysp == 0) { stop("ERROR: No year specification in 
                         date format.") }
     if (msp > 1) { stop("ERROR: Multiple specification of month in 
                         date format.") }
     if (dsp > 1) { stop("ERROR: Multiple specification of day in 
                         date format.") }

     # append month or day if not specified
     tss <- ts
     formati <- format
     if (jsp == 0) {
     if (msp == 0) { 
        tss <- paste(tss,"01",sep="")
        formati <- paste(formati,"%m",sep="")
     }
     if (dsp == 0) { 
        tss <- paste(tss,"01",sep="") 
        formati <- paste(formati,"%d",sep="")
     }
     }

     # this is necessary because strptime() returns NA otherwise
     as.POSIXct(strptime(tss,format=formati),tz="GMT")
}
