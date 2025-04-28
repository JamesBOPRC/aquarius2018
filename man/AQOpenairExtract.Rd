\name{AQOpenairExtract}
\alias{AQOpenairExtract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AQOpenairExtract
}
\description{
Extracts data from Aquarius in format requried for the Openair package
}
\usage{
AQOpenairExtract(locationid, freq, start = "", end = "")

Example -- AQOpenairExtract('EP079602','DayMean')
Extracts all data from 'EP079602' at daily frequency

Example LocationIDs

EP079602 - Whareroa Marae
EP045607 - Bridge Marina
EP116688 - Totara Street
EK171423 - Edmund Road

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{locationid}{ -- Aquarius LocationID
}
  \item{freq}{ -- frequency of data required ("Primary","HourMean" or "DayMean")
}
  \item{start}{ -- Optional argument for start of data
}
  \item{end}{ -- Optional argument for end of data
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Darren Gerretzen}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
