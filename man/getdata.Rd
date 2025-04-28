\name{getdata}
\alias{getdata}
\title{
getdata
}
\description{
Returns a dataframe of all data for a given AQDataID
}
\usage{
getdata('AQDataID', start = '', end = '')
}
\arguments{
  \item{AQDataID}{- Required - Example = 'N (Tot).LabResult@OK300616'. All DataId's at a location can be found using the datasets() function.
}
  \item{Start}{- Optional - in format 'YYYY-MM-DD'
}
  \item{End}{- Optional - in format 'YYYY-MM-DD'
}
  \item{MetadataExport}{- Optional - Default = TRUE - if MetadataExport = FALSE returned dataframe excludes LocationID, LocationName, Quality, Approvals, Qualifiers, Parameter and Units
}
  \item{LabMeta}{- Optional - Default = FALSE - if LabMeta = TRUE returned dataframe includes Method, Collection Method, Upper/Lower Detection Limits, Lab and Sample/Project IDs.
}
  \item{FieldObs}{- Optional - Default = FALSE - if FieldObs = TRUE returned dataframe includes field observations - these include "Cloud_Cover", "Num_Swimmers", "Tide_Cycle", "Tide", "Wind_Direction", "Wind_Speed", "Weather_Today", "Weather_Yest", "Surface_Cond", "Bore_Type", "Bore_Number", "Bore_Collect_Method", "Bore_Sampled_From", "Bore_Pump_Rate", "Bore_Pump_Duration", "Bore_Protocol", "Bore_Probe", "Odour" and "Samp_Comment"
}
  \item{IncludeGapMarkers}{- Optional - Default = FALSE - if IncludeGapMarkers = TRUE returned dataframe includes gapmarkers as determined by the gaptolerance setting of the dataset. Gap marker timestamp is set at the midpoint of the gap.
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
\examples{
ExampleData = getdata('N (Tot).LabResult@OK300616', LabMeta = T, FieldObs = T, start = '2018-01-01')
}
\author{
Darren Gerretzen
}

