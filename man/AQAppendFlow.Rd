\name{AQAppendFlow}
\alias{AQAppendFlow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AQAppendFlow
}
\description{
Function to append flow to multiextract.
}
\usage{
AQAppendFlow(wqdata, flowdata,bracket)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wqdata}{
A water quality datafreame in the format: AquariusID, Site Name, Time, Parameter 1:x -- directly inserting a AQMultiExtract output will work

}
\item{flowdata}{
Flow data in the format:AquariusID, Site Name, Time, Flow -- directly inserting a getdata(DataId, MetadataExport = TRUE) will work

}

\item{bracket}{
The bracket (in days) that is acceptable to match time stamps.  E.g. bracket = 5 means that any flow found within 5 days will be matched to the WQ timestamp.  If more than one flow value exists within the bracket, the one closest to the WQ timestamp will be used.

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
James Dare
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
