\name{getdataraw}
\alias{getdataraw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getdataraw
}
\description{
Returns a dataframe of all RAW data for a given AQDataID (almost exactly the same as main getdata function except no option to IncludeGapMarkers)
}
\usage{
getdataraw(dataid, start = "", end = "", MetadataExport = TRUE, LabMeta = FALSE, FieldObs = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataid}{
%%     ~~Describe \code{dataid} here~~
}
  \item{start}{
%%     ~~Describe \code{start} here~~
}
  \item{end}{
%%     ~~Describe \code{end} here~~
}
  \item{MetadataExport}{
%%     ~~Describe \code{MetadataExport} here~~
}
  \item{LabMeta}{
%%     ~~Describe \code{LabMeta} here~~
}
  \item{FieldObs}{
%%     ~~Describe \code{FieldObs} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dataid, start = "", end = "", MetadataExport = TRUE,
    LabMeta = FALSE, FieldObs = FALSE)
{
    auth = getauth()
    emptysmalldf = data.frame(Timestamp = character(), Value = character(),
        stringsAsFactors = FALSE)
    emptybigdf = data.frame(Site = character(), LocationName = character(),
        Timestamp = character(), Value = character(), Quality = character(),
        Approval = character(), Qualifiers = character(), Parameter = character(),
        Unit = character(), Comments = character(), stringsAsFactors = FALSE)
    if (nchar(start) > 0) {
        start = paste(start, "+12:00", sep = "")
    }
    if (nchar(end) > 0) {
        end = paste(end, "+12:00", sep = "")
    }
    locationid = strsplit(dataid, "@")[[1]][2]
    label = strsplit((strsplit(dataid, "@")[[1]][1]), ".", fixed = TRUE)[[1]][2]
    parameter = strsplit((strsplit(dataid, "@")[[1]][1]), ".",
        fixed = TRUE)[[1]][1]
    if (label == "Field Visits") {
        sets = datasets(locationid)
        sets = sets[sets$DataId == dataid, ]
        if (nrow(sets) == 0) {
            print(paste("Requested DataId does not exist - Please review your DataId -",
                dataid))
            if (MetadataExport == TRUE) {
                dater = emptybigdf
            }
            else {
                dater = emptysmalldf
            }
        }
        else {
            dater = getfieldvisitdata(parameter, locationid,
                start = start, end = end)
        }
    }
    else {
        sets = datasets(locationid)
        if (nrow(sets) == 0) {
            print(paste("Location either has no datasets or does not exist -- Please review your DataId -",
                dataid))
            if (MetadataExport == TRUE) {
                dater = emptybigdf
            }
            else {
                dater = emptysmalldf
            }
        }
        else {
            uniqueid = sets$UniqueId[sets$DataId == dataid]
            if (length(uniqueid) == 0) {
                print(paste("Requested DataId does not exist - Please review your DataId -",
                  dataid))
                if (MetadataExport == TRUE) {
                  dater = emptybigdf
                }
                else {
                  dater = emptysmalldf
                }
            }
            else {
                url_call = "http://brcsvapp08:80/AQUARIUS/Publish/v2/GetTimeSeriesRawData"
                query = list(TimeSeriesUniqueId = uniqueid, QueryFrom = start,
                  QueryTo = end)
                dater = GET(url_call, query = query)
                dater = fromJSON(content(dater, "text"))
                grades = dater$Grades
                approvals = dater$Approvals
                qualifiers = dater$Qualifiers
                locationid = dater$LocationIdentifier
                parameter = dater$Parameter
                units = dater$Unit
                dater = dater$Points
                if (is.data.frame(dater)) {
                  dater$Value = dater$Value$Numeric
                  dater$Timestamp = as.POSIXct(strptime(paste(substr(dater$Timestamp,
                    1, 10), substr(dater$Timestamp, 12, 19)),
                    "\%Y-\%m-\%d \%H:\%M:\%S"), tz = "Etc/GMT+12")
                  if (MetadataExport == TRUE) {
                    dater = appendjsonmetadata(dater, grades,
                      "Quality", "GradeCode")
                    dater = appendjsonmetadata(dater, approvals,
                      "Approval", "LevelDescription")
                    dater = appendjsonmetadataqualifiers(dater,
                      qualifiers, "Qualifiers", "Identifier")
                    location_name = getlocationdescriptions(locationid)
                    dater$Parameter = parameter
                    dater$Unit = units
                    dater$Site = locationid
                    dater$LocationName = location_name
                    dater <- dater[c("Site", "LocationName",
                      "Timestamp", "Value", "Quality", "Approval",
                      "Qualifiers", "Parameter", "Unit")]
                  }
                  if (LabMeta == TRUE) {
                    if (!"laberrorlog" \%in\% ls(envir = .GlobalEnv)) {
                      laberrorlog <<- get_lab_error_log()
                    }
                    locresultanalysis = resultanalysis(locationid)
                    locresultanalysis$simple_sampled_datetime = format(locresultanalysis$sampled_datetime,
                      "\%Y-\%m-\%d \%H:\%M")
                    locresultanalysis$Lab = unlist(lapply(locresultanalysis$ANALYSIS,
                      FUN = analysis_to_lab))
                    if (nrow(locresultanalysis) < 1) {
                      locresultanalysis$Lab = character(0)
                    }
                    dater$simple_sampled_datetime = format(dater$Timestamp,
                      "\%Y-\%m-\%d \%H:\%M")
                    dater = merge(dater, locresultanalysis, by.x = c("simple_sampled_datetime",
                      "Parameter", "Value"), by.y = c("simple_sampled_datetime",
                      "Identifier", "ENTRY"), all.x = TRUE)
                    dater = merge(dater, laberrorlog, by.x = c("Timestamp",
                      "Site"), by.y = c("Date.Time", "AquariusID"),
                      all.x = TRUE)
                    if (FieldObs == TRUE) {
                      dater <- dater[c("Site", "LocationName",
                        "Timestamp", "Value", "Quality", "Approval",
                        "Qualifiers", "Parameter", "Unit", "Sample_Number",
                        "Project_Id", "C_LDL", "C_UDL", "C_METHOD_REFERENCE",
                        "COLLECTION_METHOD", "Lab", "Error",
                        "Comment", "ACTION", "Samp_Comment",
                        "Cloud_Cover", "Num_Swimmers", "Tide_Cycle",
                        "Tide", "Wind_Direction", "Wind_Speed",
                        "Weather_Today", "Weather_Yest", "Surface_Cond",
                        "Bore_Type", "Bore_Number", "Bore_Collect_Method",
                        "Bore_Sampled_From", "Bore_Pump_Rate",
                        "Bore_Pump_Duration", "Bore_Protocol",
                        "Bore_Probe", "Odour")]
                    }
                    else {
                      dater <- dater[c("Site", "LocationName",
                        "Timestamp", "Value", "Quality", "Approval",
                        "Qualifiers", "Parameter", "Unit", "Sample_Number",
                        "Project_Id", "C_LDL", "C_UDL", "C_METHOD_REFERENCE",
                        "COLLECTION_METHOD", "Lab", "Error",
                        "Comment", "ACTION")]
                    }
                    colnames(dater)[colnames(dater) == "C_METHOD_REFERENCE"] <- "Method"
                    colnames(dater)[colnames(dater) == "COLLECTION_METHOD"] <- "CollectionMethod"
                    colnames(dater)[colnames(dater) == "C_LDL"] <- "LowerDetectionLimit"
                    colnames(dater)[colnames(dater) == "C_UDL"] <- "UpperDetectionLimit"
                  }
                }
                else {
                  print(paste(dataid, " has no data for requested period",
                    sep = ""))
                  if (MetadataExport == TRUE) {
                    dater = emptybigdf
                  }
                  else {
                    dater = emptysmalldf
                  }
                }
            }
        }
        colnames(dater)[colnames(dater) == "Timestamp"] <- "Time"
    }
    return(dater)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
