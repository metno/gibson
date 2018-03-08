\name{gibson_textfile}
\alias{gibson_textfile}
\title{get metadata and observations from text files}
\description{
  Read observations from a text files stored somewhere on the disk.
}
\usage{
gibson_textfile(date.str=NULL,
               format=NULL,
               path="",
               path.date="yyyy/mm/dd"
               fbname=""
               fbname.date="yyyymmddThhZ"
               fext="txt",
               fsep=";",
               selout=c("lat","lon","elev"),
               na=-999,
               na.rm=T)

}
\arguments{
  \item{date}{a date-time string, such as for example \code{"2017.01.01"}}
  \item{format}{the format to be used for the output string. For the
          list of items see the help page of \code{strptime}}
  \item{path}{string containing the path used to construct the full file name}
  \item{path.date}{string containing that part of the path that depends on the date}
  \item{fbname}{string with the base name of the file name}
  \item{fbname.date}{string with the part of the base name of the file name that depends on the date}
  \item{fext}{string with file extension}
  \item{fsep}{character used to separate fields in the text file}
  \item{selout}{vector of strings with the column names to write into the output file}
  \item{na}{numeric defining the NA value}
  \item{na.rm}{logical, decide whether NAs are included in the output}
}
\details{
  The name of the file is set to path/path.date/fbname+fbname.date+fext.

  The format of the input is something like:

lat;lon;elev;FF;PR;TA;DG;DD;RR_1;FG;UU
59.520877;10.698517;38;0.56;1006.20;-4.60;103;90;-999;1.94;81
58.785390;5.624396;43;-999;994.70;-1.20;-999;-999;-999;-999;65
61.722195;17.104938;7;0.56;1005.00;-3.90;325;345;-999;1.39;98
  ...

  so to get metadata and hourly precipitation use \code{selout=c("lat","lon","elev","RR_1")}.

  Possible variables are: FF hourly wind speed; PR hourly atmospheric pressure; 
  TA hourly temperature; DG direction of the wind gust; DD wind direction;
  RR_1 hourly total precipitation; FG wind gust; UU relative humidity.
  
  The values refers to the last 10 minutes of the hour used as time stamp.
  Exceptions are: RR_1 that is the total precipitation for the hour; ....


  These function invoke the function \code{replaceDate}.
}
\value{
  a data frame with the columns specified by selout and the value read from the input text file.
}
\author{ Cristian Lussana }

\seealso{ \code{\link{replaceDate}} }
\examples{
# get precipitation and temperature data (not NAs)
data <- gibson_textfile(date.str="2018.03.04.12",format="\%Y.\%m.\%d.\%H",selout=c("lat","lon","elev","RR_1","TA"))

}
% at least one, from /Library/Frameworks/R.framework/Versions/2.1.1/Resources/doc
\keyword{ observation, read, text, file }

