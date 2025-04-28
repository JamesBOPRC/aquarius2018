\name{Summarise_15Mins}
\alias{Summarise_15Mins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarise into 15 minute intervals
}
\description{
Summarise a continuous dataset into 15 minute intervals by total or means.
}
\usage{
Summarise_15Mins(Data, Method, Gap_Fill)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
Continuous data extracted using the 'getdata' function.
}
  \item{Method}{
Either 'Mean' or 'Total' depending on whether data is to be averaged by 15 minute intervals or summed (totalled).
}
  \item{Gap_Fill}{
(Optional) FALSE (default) will summarise the continuous dataset into 15 minute intervals with data gaps preserved. TRUE will fill data gaps with missing time interval stamps and populate the value field with '0'.
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
rainfall <- getdata("PPT.Primary@EL051775",start="01-01-2015",end = "01-01-2016")
output <- Summarise_15Mins(rainfall, Method = "Mean", Gap_Fill = T)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
