\name{AQMultiExtract}
\alias{AQMultiExtract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AQMultiExtract
}
\description{
Function returns a multi-parameter, multi-site dataframe of discrete Field Visit datasets.

Was designed to export Water Quality data in an easy use functional way.

Inputs are two lists - sites and parameters
}
\usage{
AQMultiExtract(sitelist, parameters, start, end)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sitelist}{
Can be entered either as single site list eg. "AQSiteID" or as multiple site lists eg. c("AQSiteID1","AQSiteID2")
}
  \item{parameters}{
A list of parameters in the form that can be seen in Aquarius Springboard

The function LocationWQParameters("AQLocationID") will return a list of all available Water Quality parameters at a particular location. This can be input directly into the function
}
\item{start}{
(Optional)  The start date for the extraction in 'YYYY-MM-DD'
}
\item{end}{
(Optional)  The end date for the extraction in 'YYYY-MM-DD'
}
\item{site_merge}{
(Optional)  Default = FALSE. Will merge WQ sites that have changed location and are stored under a different site name.  This argument refers to a hard coded table of known site changes.  The new and old SiteID can be used to query data if this option is enabled, with both inputs producing the same dataset.  Output will be labelled with the with the most up-to-date SiteID and name even though the input SiteID may differ.

}
\item{AppendFlow}{
(Optional)  Default = FALSE. Appends Discharge at SampleTime data plus metadata of DischargeLocation, DischargeType and DischargeTime. Is currently only designed to work for NERMN locations which have Discharge at SampleTime dataset - for more info on this process please contact DataServices
}

\item{Qualifiers}{
(Optional)  If field is added then all results are filtered by a partial string match. For example if Qualifiers = 'Recreational' then all results containing 'Recreational' in the qualifier string ('Recreational', 'Recreational Repeats') will be returned. Default does not filter.
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

