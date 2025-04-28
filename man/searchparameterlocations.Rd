\name{searchparameterlocations}
\alias{searchparameterlocations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
searchparameterlocations
}
\description{
Returns timeseries metadata for all datasets with particular input parameter -- this includes location identifier

Input is Aquarius Parameter Display ID as seen in Aquarius Springboard, as a string (ie. in quotation marks)

e.g. searchparameterlocations("Discharge")
will return all timeseries metadara where any Discharge dataset is present, no matter what the label - Primary, Field Visits, etc
}
\usage{
searchparameterlocations(display_id)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{display_id}{
Aquarius Parameter Display ID as seen in Aquarius Springboard
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

