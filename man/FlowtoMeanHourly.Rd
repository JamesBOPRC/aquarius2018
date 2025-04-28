\name{FlowtoMeanHourly}
\alias{FlowtoMeanHourly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FlowtoMeanHourly
}
\description{
Converts continuous flow output from the 'getdata' function, to hourly mean flow.
}
\usage{
FlowtoMeanHourly(Flow, Gap_Fill = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Flow}{
getdata() flow dataframe
}
  \item{Gap_Fill}{
(Optional) TRUE will fill data gaps with missing time interval stamps and populate the value field with '0'. Default = FALSE
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
Darren Gerretzen
}
