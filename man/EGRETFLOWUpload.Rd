\name{EGRETFLOWUpload}
\alias{EGRETFLOWUpload}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
EGRETFLOWUpload
}
\description{
This function converts mean daily flow data provided by the aquarius 'getdata' function, to the format required for EGRET analysis.  Input is a dataframe output using 'getdata'.  'qUnit' is the unit of flow with '2' meaning cubic metres per second.  Use 'printqUnitCheatSheet()' for all flow unit codes.

The getdata argument MetadataExport is not required to be set to TRUE

"timecolumnname" and "flowcolumnname" must be specified -- this is to allow for the direct import from FlowtoMeanDaily - timecolumnname = "Day" and flowcolumnname = "Flow".

From getdata extract - timecolumnname = "Time" and flowcolumnname = "Value".
}
\usage{
EGRETFLOWUpload(data, timecolumnname, flowcolumnname, qUnit = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{qUnit}{
%%     ~~Describe \code{qUnit} here~~
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

