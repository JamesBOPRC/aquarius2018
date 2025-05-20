getauth <- function() {
  require('httr')
  require('jsonlite')
  body = list(Username ="APIAccess",EncryptedPassword = "itsdatatime")
  url_call = "http://brcsvapp08:80/AQUARIUS/Publish/v2/session"
  token = POST(url_call, body = body)
  return (token)}

labware_dbcall <- function(SQLstring) {
  require(RODBC)
  labwareconnection <- odbcDriverConnect('driver={SQL Server};server=BRCSVSQL14;database=lims;uid=LabwareReader;pwd=R3ader')
  df <- sqlQuery(labwareconnection, SQLstring, as.is = TRUE)
  close(labwareconnection)
  return(df)
}

getsampleids <- function() {
  sample_ids = labware_dbcall("SELECT 	distinct 	SAMPLED_DATE + CASE WHEN ISDATE(C_SAMPLED_NZST) <> 0 THEN CONVERT(TIME, C_SAMPLED_NZST) WHEN RIGHT(C_SAMPLED_NZST, 4) IN ('a.m.', 'p.m.') THEN CONVERT(TIME, LEFT(C_SAMPLED_NZST, 8)) ELSE CONVERT(TIME, '00:00:00') END AS sampled_datetime, 	SAMPLING_POINT as sampling_point, 	PROJECT as project_id, 	TEXT_ID as sample_id FROM 	SAMPLE WHERE 	SAMPLING_POINT is not null")
  sample_ids$sampled_datetime = as.POSIXct(strptime(paste(substr(sample_ids$sampled_datetime,1,10),substr(sample_ids$sampled_datetime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
  return(sample_ids)
}

resultanalysis <- function(locationid){
  SQLQuery = paste("SELECT METADATA.PROJECT as Project_Id, METADATA.TEXT_ID as Sample_Number, METADATA.ENTRY, METADATA.ANALYSIS, METADATA.C_DETECTION_LIMIT, METADATA.C_LDL, METADATA.C_UDL, METADATA.SAMPLED_DATETIME as sampled_datetime, METADATA.C_AQUARIUS_PARAMETER, METADATA.C_METHOD_REFERENCE, METADATA.COLLECTION_METHOD, METADATA.C_CLOUD_COVER as Cloud_Cover, METADATA.C_NUM_SWIMMERS as Num_Swimmers, METADATA.C_TIDE_CYCLE as Tide_Cycle, METADATA.C_TIDE as Tide, METADATA.C_WIND_DIRECTION as Wind_Direction, METADATA.C_WIND_SPEED as Wind_Speed, METADATA.C_WEATHER_TODAY as Weather_Today, METADATA.C_WEATHER_YEST as Weather_Yest, METADATA.C_SURFACE_COND as Surface_Cond, LIMS_HTML_NOTES.NOTE_CONTENTS as Samp_Comment, METADATA.C_BORE_TYPE as Bore_Type, METADATA.C_BORE_NUMBER as Bore_Number, METADATA.C_BORE_COLLECTMETHOD as Bore_Collect_Method, METADATA.C_BORE_SAMPLEDFROM as Bore_Sampled_From, METADATA.C_BORE_PUMPRATE as Bore_Pump_Rate, METADATA.C_BORE_PUMPDURATION as Bore_Pump_Duration, METADATA.C_BORE_PROTOCOL as Bore_Protocol, METADATA.C_BORE_PROBE as Bore_Probe, METADATA.C_ODOUR as Odour FROM (SELECT distinct RESULT.ENTRY, RESULT.ANALYSIS, RESULT.C_DETECTION_LIMIT, RESULT.C_LDL, RESULT.C_UDL, SAMPLE_TIMES.SAMPLED_DATETIME, SAMPLE_TIMES.PROJECT, SAMPLE_TIMES.TEXT_ID, ANALYSIS.C_AQUARIUS_PARAMETER, ANALYSIS.C_METHOD_REFERENCE, ANALYSIS.COLLECTION_METHOD,SAMPLE_TIMES.C_CLOUD_COVER, SAMPLE_TIMES.C_NUM_SWIMMERS, SAMPLE_TIMES.C_TIDE_CYCLE, SAMPLE_TIMES.C_TIDE, SAMPLE_TIMES.C_WIND_DIRECTION, SAMPLE_TIMES.C_WIND_SPEED, SAMPLE_TIMES.C_WEATHER_TODAY, SAMPLE_TIMES.C_WEATHER_YEST, SAMPLE_TIMES.C_SURFACE_COND, SAMPLE_TIMES.C_BORE_TYPE, SAMPLE_TIMES.C_BORE_NUMBER, SAMPLE_TIMES.C_BORE_COLLECTMETHOD, SAMPLE_TIMES.C_BORE_SAMPLEDFROM, SAMPLE_TIMES.C_BORE_PUMPRATE, SAMPLE_TIMES.C_BORE_PUMPDURATION, SAMPLE_TIMES.C_BORE_PROTOCOL, SAMPLE_TIMES.C_BORE_PROBE, SAMPLE_TIMES.C_ODOUR, SAMPLE_TIMES.T_NOTE FROM (SELECT NON_ORIGINAL.SAMPLE_NUMBER, ORIGINAL_TIME.SAMPLED_DATETIME, ORIGINAL_TIME.PROJECT, ORIGINAL_TIME.TEXT_ID, ORIGINAL_TIME.C_CLOUD_COVER, ORIGINAL_TIME.C_NUM_SWIMMERS, ORIGINAL_TIME.C_TIDE_CYCLE, ORIGINAL_TIME.C_TIDE, ORIGINAL_TIME.C_WIND_DIRECTION, ORIGINAL_TIME.C_WIND_SPEED, ORIGINAL_TIME.C_WEATHER_TODAY, ORIGINAL_TIME.C_WEATHER_YEST, ORIGINAL_TIME.C_SURFACE_COND, ORIGINAL_TIME.C_BORE_TYPE, ORIGINAL_TIME.C_BORE_NUMBER, ORIGINAL_TIME.C_BORE_COLLECTMETHOD, ORIGINAL_TIME.C_BORE_SAMPLEDFROM, ORIGINAL_TIME.C_BORE_PUMPRATE, ORIGINAL_TIME.C_BORE_PUMPDURATION, ORIGINAL_TIME.C_BORE_PROTOCOL, ORIGINAL_TIME.C_BORE_PROBE, ORIGINAL_TIME.C_ODOUR, ORIGINAL_TIME.T_NOTE FROM (SELECT SAMPLE_NUMBER, ORIGINAL_SAMPLE FROM SAMPLE WHERE SAMPLE_NUMBER not in ( SELECT distinct ORIGINAL_SAMPLE FROM [lims].[dbo].SAMPLE WHERE SAMPLING_POINT = '",locationid,"' and ORIGINAL_SAMPLE is not null) and sample.SAMPLING_POINT = '",locationid,"') NON_ORIGINAL JOIN (SELECT SAMPLED_DATE + CASE WHEN ISDATE(C_SAMPLED_NZST) <> 0 THEN CONVERT(TIME, C_SAMPLED_NZST) WHEN RIGHT(C_SAMPLED_NZST, 4) IN ('a.m.', 'p.m.') THEN CONVERT(TIME, LEFT(C_SAMPLED_NZST, 8)) ELSE CONVERT(TIME, '00:00:00') END AS SAMPLED_DATETIME, PROJECT, TEXT_ID, SAMPLE_NUMBER, C_CLOUD_COVER, C_NUM_SWIMMERS, C_TIDE_CYCLE, C_TIDE, C_WIND_DIRECTION, C_WIND_SPEED, C_WEATHER_TODAY, C_WEATHER_YEST, C_SURFACE_COND, C_BORE_TYPE, C_BORE_NUMBER, C_BORE_COLLECTMETHOD, C_BORE_SAMPLEDFROM, C_BORE_PUMPRATE, C_BORE_PUMPDURATION, C_BORE_PROTOCOL, C_BORE_PROBE, C_ODOUR, T_NOTE FROM SAMPLE WHERE SAMPLING_POINT = '",locationid,"' ) ORIGINAL_TIME ON NON_ORIGINAL.ORIGINAL_SAMPLE = ORIGINAL_TIME.SAMPLE_NUMBER) SAMPLE_TIMES JOIN RESULT ON RESULT.SAMPLE_NUMBER = SAMPLE_TIMES.SAMPLE_NUMBER JOIN (SELECT distinct MAX_ANALYSIS.NAME, ANALYSIS.C_AQUARIUS_PARAMETER, ANALYSIS.C_METHOD_REFERENCE, ANALYSIS.COLLECTION_METHOD FROM (SELECT NAME, MAX(VERSION) AS Latest FROM [lims].[dbo].ANALYSIS AS ANALYSIS_1 GROUP BY NAME) MAX_ANALYSIS JOIN ANALYSIS ON MAX_ANALYSIS.NAME = ANALYSIS.NAME AND MAX_ANALYSIS.Latest = ANALYSIS.VERSION WHERE ANALYSIS.C_AQUARIUS_PARAMETER is not null) ANALYSIS ON ANALYSIS.NAME = RESULT.ANALYSIS WHERE RESULT.REPORTABLE = 'T') METADATA LEFT JOIN LIMS_HTML_NOTES ON METADATA.T_NOTE = LIMS_HTML_NOTES.NOTE_ID", sep = "")
  df = labware_dbcall(SQLQuery)
  df$sampled_datetime = as.POSIXct(strptime(paste(substr(df$sampled_datetime,1,10),substr(df$sampled_datetime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
  param = parameters_provisioning()
  param = param[,c('ParameterId','Identifier')]
  df = merge(df,param, by.x = "C_AQUARIUS_PARAMETER", by.y = "ParameterId")
  df$ENTRY = gsub('<','',df$ENTRY)
  df$ENTRY = gsub('>','',df$ENTRY)
  df$ENTRY = as.numeric(df$ENTRY)
  return(df)}

get_lab_error_log <- function() {
  require(httr)
  #download.file("https://objective.envbop.net/id:A3049201/document/versions/latest", destfile = "\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\R\\configuration_files\\lab_error_log.csv", mode = "wb")

  error_log <-GET("https://objective.envbop.net/id:A3049201/document/versions/latest", authenticate(":",":",type="gssnegotiate"))
  writeBin(content(error_log, "raw"), "\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\R\\configuration_files\\lab_error_log.csv")

  df <- read.csv("\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\R\\configuration_files\\lab_error_log.csv", 1)
  df$Date.Time <- as.POSIXct(strptime(df$Date.Time,"%d/%m/%Y %H:%M:%S"),tz = "Etc/GMT+12", "%Y-%m-%d %H:%M:%S")
  return(df)
}

analysis_to_lab <- function(analysis){
  lab = ""
  if (grepl("X_RJH", analysis, fixed=TRUE)){
    lab = "Hills"
  }
  else if(grepl("X_NIWA", analysis, fixed=TRUE)){
    lab = 'NIWA'
  }
  else if(grepl("X_WU", analysis, fixed=TRUE)){
    lab = 'Waikato Uni'
  }
  else if(grepl("X_EU", analysis, fixed=TRUE)){
    lab = "Eurofins"
  }
  else if(grepl("X_LANDCARE", analysis, fixed=TRUE)){
    lab = "Landcare"
  }
  else if(grepl("X_AQ", analysis, fixed=TRUE)){
    lab = "AsureQuality"
  }
  else if(grepl("X_DA", analysis, fixed=TRUE)){
    lab = "Dowdell and Associates"
  }
  else if(grepl("X_WC", analysis, fixed=TRUE)){
    lab = "Watercare"
  }
  else{
    lab = "BoPRC"
  }
  return(lab)
}

searchprojectcodes <- function(projectcode){
  SQLQuery = paste("SELECT 	DISTINCT ANALYSIS, 	UNITS.C_AQUARIUS_UNITS, 	ANALYSIS.C_AQUARIUS_PARAMETER, 	SAMPLE.SAMPLING_POINT, 	SAMPLE.C_PROJECT_CODE, 	RESULT.STATUS FROM 	[lims].[dbo].RESULT INNER JOIN 	[lims].[dbo].UNITS ON 	RESULT.UNITS = UNITS.UNIT_CODE INNER JOIN 	[lims].[dbo].ANALYSIS ON 	RESULT.ANALYSIS = ANALYSIS.NAME INNER JOIN 	( SELECT NAME , MAX(VERSION) AS Latest FROM   [lims].[dbo].ANALYSIS AS ANALYSIS_1 GROUP BY NAME ) AS LA ON 	ANALYSIS.NAME = LA.NAME 	AND ANALYSIS.VERSION = LA.Latest INNER JOIN 	 [lims].[dbo].SAMPLE ON 	SAMPLE.SAMPLE_NUMBER = RESULT.SAMPLE_NUMBER WHERE 	RESULT.ENTRY IS NOT NULL 	AND RESULT.REPORTABLE = 'T' 	AND C_AQUARIUS_PARAMETER IS NOT NULL 	AND C_AQUARIUS_UNITS IS NOT NULL 	AND SAMPLING_POINT IS NOT NULL 	AND RESULT.STATUS in ('A','C') 	AND C_PROJECT_CODE LIKE '%",projectcode,"%'",sep = "")
  df = labware_dbcall(SQLQuery)
  param = parameters_provisioning()
  df = merge(df, param, by.x = c("C_AQUARIUS_PARAMETER","C_AQUARIUS_UNITS"), by.y = c("ParameterId","UnitIdentifier"))
  df$DATAID = paste(df$DisplayName,".LabResult@",df$SAMPLING_POINT, sep = "")
  df = df[,c("SAMPLING_POINT","DATAID","C_PROJECT_CODE")]
  return(df)
}

getparameters <- function() {
  getauth()
  require('jsonlite')
  require(httr)
  url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetParameterList'
  datas = GET(url_call)
  datalist = fromJSON(content(datas,"text"))
  return (datalist$Parameters)}

parameters_provisioning <- function() {
  auth = getauth()
  require('jsonlite')
  require(httr)
  url_call = 'http://brcsvapp08:80/AQUARIUS/Provisioning/v1/parameters'
  datas = GET(url_call)
  datalist = fromJSON(content(datas,"text"))
  return (datalist$Results)}

timeseriesmetadata <- function(uniqueid){
  require(httr)
  auth = getauth()
  url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetTimeSeriesCorrectedData'
  query = list(TimeSeriesUniqueId = uniqueid, GetParts = 'MetadataOnly')
  dater = GET(url_call, query = query)
  dater = fromJSON(content(dater,"text"))
  return(dater)}

datasets <- function (LocationIdentifier) {
  require(httr)
  auth = getauth()
  url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetTimeSeriesDescriptionList'
  query = list(LocationIdentifier = LocationIdentifier)
  datasets = GET(url_call, query = query)
  datasets = fromJSON(content(datasets,"text"))
  datasets_template = data.frame(Identifier = character(), Unit = character(), TimeSeriesType = character(),
                                 Publish = logical(), ExtendedAttributes = character(), UniqueId = character(),
                                 UtcOffset = integer(), Label = character(), ComputationIdentifier = character(),
                                 Thresholds = character(), LocationIdentifier = character(), UtcOffsetIsoDuration = character(),
                                 Comment = character(), ComputationPeriodIdentifier = character(), Parameter = character(),
                                 LastModified = character(), Description = character(), SubLocationIdentifier = character(), stringsAsFactors = FALSE)
  if (length(datasets$TimeSeriesDescriptions) > 0){
    datasets = datasets$TimeSeriesDescriptions
    datasets = merge(datasets, datasets_template, all = TRUE)
  }
  else{
    datasets = datasets_template
  }
  fieldvisitdatasets = get_fieldvisit_datasets()
  fieldvisitdatasets = fieldvisitdatasets[fieldvisitdatasets$LocationIdentifier == LocationIdentifier,]
  datasets = merge(datasets, fieldvisitdatasets, all = TRUE)
  colnames(datasets)[colnames(datasets) == 'Identifier'] <- 'DataId'
  datasets = datasets[order(datasets$LocationIdentifier),]
  rownames(datasets) = NULL
  param = getparameters()
  param = param[,c('Identifier','DisplayName')]
  colnames(param)[colnames(param)=='Identifier'] <- 'Parameter'
  datasets <- merge(datasets,param)
  datasets$RawStartTime = as.POSIXct(strptime(paste(substr(datasets$RawStartTime,1,10),substr(datasets$RawStartTime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
  datasets$RawEndTime = as.POSIXct(strptime(paste(substr(datasets$RawEndTime,1,10),substr(datasets$RawEndTime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
  datasets$CorrectedStartTime = as.POSIXct(strptime(paste(substr(datasets$CorrectedStartTime,1,10),substr(datasets$CorrectedStartTime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
  datasets$CorrectedEndTime = as.POSIXct(strptime(paste(substr(datasets$CorrectedEndTime,1,10),substr(datasets$CorrectedEndTime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
  datasets$LastModified = as.POSIXct(strptime(paste(substr(datasets$LastModified,1,10),substr(datasets$LastModified,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
  return (datasets)}

LocationWQParameters <- function(locationid) {
  sets <- datasets(locationid)
  #|sets$Parameter %in% c('TSS','Water Temp')) -- to include if we go for Field Visit data also
  lab_sets = subset(sets, Label %in% c('LabResult','FieldResult'))
  param <- unique(lab_sets$Parameter)
  if (length(param) < 1){
    print(paste("Warning - Locationid",locationid,"has no WQ parameters"))
  }
  return (param)
}

get_all_locations <- function() {
  location_metadata = read.csv("\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\Aquarius Location Metadata\\Aquarius_Location_Metadata.csv")
  return (location_metadata)
}

searchlocationid <- function(LocationIdentifier) {
  require(httr)
  auth = getauth()
  url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetLocationData'
  query = list(LocationIdentifier = LocationIdentifier)
  locations = GET(url_call, query = query)
  locations = fromJSON(content(locations,"text"))
  if (is.character(locations$LocationName)){
    finaldf = data.frame()
    extended = locations$ExtendedAttributes
    attributeslist = c("LocationName","Description","Identifier","UniqueId","LocationType","IsExternalLocation","Latitude","Longitude","ElevationUnits","Elevation","UtcOffset")
    for (i in 1:length(attributeslist)){
     name = attributeslist[i]
     value = locations[name]
     if (is.null(value[[1]])){
       value = ''
     }
     finaldf[1,name] = value
    }
    for (i in 1:nrow(extended)){
      name = extended[i,"Name"]
      value = extended[i,"Value"]
      finaldf[1,name] = value
    }}
  else{
    print(paste(locations$ResponseStatus$Message,' - SKIPPING'), sep = '')
    finaldf = data.frame()
  }
  return (finaldf)}

searchlocationnames <- function(wildcard){
  wildcard = tolower(wildcard)
  all_loc <- get_all_locations()
  all_loc$locationnamelower = tolower(all_loc$locationname)
  output <- all_loc[grep(wildcard,all_loc$locationnamelower),]
  output = output[,!(names(output) %in% c('locationnamelower'))]
  return(output)
}

getlocationdescriptions <- function(LocationIdentifier) {
  require(httr)
  auth = getauth()
  url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetLocationDescriptionList'
  query = list(LocationIdentifier = LocationIdentifier)
  locations = GET(url_call, query = query)
  locations = fromJSON(content(locations,"text"))
  locationname = locations$LocationDescriptions$Name
  return (locationname)}

appendjsonmetadata <- function(pointsdf,metajson,columnname,valuename){

  if (class(metajson) != "list") {
    pointsdf[columnname] = ""
    ## parsed with milliseconds to get correct start and end times
    metajson$StartTime = as.POSIXct(strptime(paste(substr(metajson$StartTime,0,22), substr(metajson$StartTime,27,33),sep = ""), "%Y-%m-%dT%H:%M:%OS"), tz = "Etc/GMT+12")
    metajson$EndTime = as.POSIXct(strptime(paste(substr(metajson$EndTime,0,22), substr(metajson$EndTime,27,33),sep = ""), "%Y-%m-%dT%H:%M:%OS"), tz = "Etc/GMT+12")
    for(i in 1:nrow(metajson)) {
      metavalue = metajson[i,valuename]
      if (nrow(pointsdf[(pointsdf$Timestamp >= metajson[i,"StartTime"])&(pointsdf$Timestamp < metajson[i,"EndTime"]),][columnname]) > 0){
        pointsdf[(pointsdf$Timestamp >= metajson[i,"StartTime"])&(pointsdf$Timestamp < metajson[i,"EndTime"]),][columnname] <- metavalue
      }
    }
  }
  else{
    pointsdf[columnname] = NA
  }
  return(pointsdf)
}

appendjsonmetadataqualifiers <- function(pointsdf,metajson,columnname,valuename){
  if (class(metajson) != "list") {
    pointsdf[columnname] = ""
    ## parsed with milliseconds to get correct start and end times
    metajson$StartTime = as.POSIXct(strptime(paste(substr(metajson$StartTime,0,22), substr(metajson$StartTime,27,33),sep = ""), "%Y-%m-%dT%H:%M:%OS"), tz = "Etc/GMT+12")
    metajson$EndTime = as.POSIXct(strptime(paste(substr(metajson$EndTime,0,22), substr(metajson$EndTime,27,33),sep = ""), "%Y-%m-%dT%H:%M:%OS"), tz = "Etc/GMT+12")
    for(i in 1:nrow(metajson)) {
      metavalue = metajson[i,valuename]
      if (nrow(pointsdf[(pointsdf$Timestamp >= metajson[i,"StartTime"])&(pointsdf$Timestamp < metajson[i,"EndTime"]),][columnname]) > 0){
        pointsdf[(pointsdf$Timestamp >= metajson[i,"StartTime"])&(pointsdf$Timestamp < metajson[i,"EndTime"]),][columnname] <- paste(pointsdf[(pointsdf$Timestamp >= metajson[i,"StartTime"])&(pointsdf$Timestamp < metajson[i,"EndTime"]),][,columnname], metavalue, sep = ',')
      }
    }
  }
  else{
    pointsdf[columnname] = ''
  }
  pointsdf[columnname] <- substr(pointsdf[,columnname],2, 50000)
  return(pointsdf)
}

getdata <- function(dataid, start = '', end = '', MetadataExport = TRUE, LabMeta = FALSE, FieldObs = FALSE, IncludeGapMarkers = FALSE){
  require(httr)
  auth = getauth()
  emptysmalldf = data.frame(Timestamp = character(), Value = character(), stringsAsFactors = FALSE)
  emptybigdf = data.frame(Site = character(), LocationName = character(), Timestamp = character(), Value = character(), Quality = character(), Approval = character(), Qualifiers = character(), Parameter = character(), Unit = character(), Comments = character(), stringsAsFactors = FALSE)
  if (nchar(start) > 0){
    start = paste(start,"+12:00",sep="")
  }
  if (nchar(end) > 0){
    end = paste(end,"+12:00",sep="")
  }
  locationid = strsplit(dataid,'@')[[1]][2]
  label = strsplit((strsplit(dataid,'@')[[1]][1]),".",fixed = TRUE)[[1]][2]
  parameter = strsplit((strsplit(dataid,'@')[[1]][1]),".",fixed = TRUE)[[1]][1]
  if (label == 'Field Visits'){
    sets = datasets(locationid)
    sets = sets[sets$DataId == dataid,]
    if (nrow(sets) == 0){
      print(paste("Requested DataId does not exist - Please review your DataId -",dataid))
      if (MetadataExport == TRUE){
        dater = emptybigdf
      }
      else{
        dater = emptysmalldf
      }
    }
    else{
      dater = getfieldvisitdata(parameter,locationid, start = start, end = end)}
  }
  else{
  sets = datasets(locationid)
  if (nrow(sets) == 0){
    print(paste("Location either has no datasets or does not exist -- Please review your DataId -",dataid))
    if (MetadataExport == TRUE){
      dater = emptybigdf
    }
    else{
      dater = emptysmalldf
    }
  }
  else{
    uniqueid = sets$UniqueId[sets$DataId == dataid]
    if (length(uniqueid) == 0){
      print(paste("Requested DataId does not exist - Please review your DataId -",dataid))
      if (MetadataExport == TRUE){
        dater = emptybigdf
      }
      else{
        dater = emptysmalldf
      }
    }
    else{
      url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetTimeSeriesCorrectedData'
      query = list(TimeSeriesUniqueId = uniqueid, QueryFrom = start, QueryTo = end, IncludeGapMarkers = IncludeGapMarkers)
      dater = GET(url_call, query = query)
      dater = fromJSON(content(dater,"text"))
      grades = dater$Grades
      approvals = dater$Approvals
      qualifiers = dater$Qualifiers
      locationid = dater$LocationIdentifier
      parameter = dater$Parameter
      units = dater$Unit
      dater = dater$Points
      if (is.data.frame(dater)){
        dater$Value = dater$Value$Numeric
        dater$Timestamp = as.POSIXct(strptime(paste(substr(dater$Timestamp,1,10),substr(dater$Timestamp,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
        if (MetadataExport == TRUE){
          dater = appendjsonmetadata(dater, grades,'Quality','GradeCode')
          dater = appendjsonmetadata(dater, approvals, 'Approval', 'LevelDescription')
          dater = appendjsonmetadataqualifiers(dater, qualifiers, 'Qualifiers', 'Identifier')
          location_name = getlocationdescriptions(locationid)
          dater$Parameter = parameter
          dater$Unit = units
          dater$Site = locationid
          dater$LocationName = location_name
          dater <- dater[c("Site", "LocationName", "Timestamp", "Value", "Quality", "Approval", "Qualifiers", "Parameter", "Unit")]
        }
        if (LabMeta == TRUE){
          if (!"laberrorlog" %in% ls(envir = .GlobalEnv)){
            laberrorlog <<- get_lab_error_log()}
          locresultanalysis = resultanalysis(locationid)
          locresultanalysis$simple_sampled_datetime = format(locresultanalysis$sampled_datetime,'%Y-%m-%d %H:%M')
          locresultanalysis$Lab = unlist(lapply(locresultanalysis$ANALYSIS, FUN = analysis_to_lab))
          if (nrow(locresultanalysis) < 1){
            locresultanalysis$Lab = character(0)
          }
          dater$simple_sampled_datetime = format(dater$Timestamp,'%Y-%m-%d %H:%M')
          dater = merge(dater,locresultanalysis,by.x = c('simple_sampled_datetime','Parameter','Value'), by.y = c('simple_sampled_datetime','Identifier','ENTRY'), all.x=TRUE)
          dater = merge(dater,laberrorlog,by.x = c('Timestamp','Site'), by.y = c('Date.Time','AquariusID'), all.x=TRUE)
          if (FieldObs == TRUE){
            dater <- dater[c("Site", "LocationName", "Timestamp", "Value", "Quality", "Approval", "Qualifiers", "Parameter", "Unit", "Sample_Number", "Project_Id", "C_LDL", "C_UDL", "C_METHOD_REFERENCE","COLLECTION_METHOD","Lab","Error","Comment","ACTION","Samp_Comment","Cloud_Cover", "Num_Swimmers", "Tide_Cycle", "Tide", "Wind_Direction", "Wind_Speed", "Weather_Today", "Weather_Yest", "Surface_Cond", "Bore_Type", "Bore_Number", "Bore_Collect_Method", "Bore_Sampled_From", "Bore_Pump_Rate", "Bore_Pump_Duration", "Bore_Protocol", "Bore_Probe", "Odour")]
          }
          else{
            dater <- dater[c("Site", "LocationName", "Timestamp", "Value", "Quality", "Approval", "Qualifiers", "Parameter", "Unit", "Sample_Number", "Project_Id", "C_LDL", "C_UDL", "C_METHOD_REFERENCE","COLLECTION_METHOD","Lab","Error","Comment","ACTION")]}
          colnames(dater)[colnames(dater)=="C_METHOD_REFERENCE"] <- "Method"
          colnames(dater)[colnames(dater)=="COLLECTION_METHOD"] <- "CollectionMethod"
          colnames(dater)[colnames(dater)=="C_LDL"] <- "LowerDetectionLimit"
          colnames(dater)[colnames(dater)=="C_UDL"] <- "UpperDetectionLimit"}
        if (IncludeGapMarkers == TRUE){
          if(nrow(dater[is.na(dater$Value),]) > 0){
            dater[is.na(dater$Value),]$Quality = "100"}
        }
      }
      else{
        print(paste(dataid,' has no data for requested period', sep = ''))
        if (MetadataExport == TRUE){
          dater = emptybigdf
        }
        else{
          dater = emptysmalldf
        }
      }
    }
  }
  colnames(dater)[colnames(dater)=="Timestamp"] <- "Time"}
  return (dater)
}

getdataraw <- function(dataid, start = '', end = '', MetadataExport = TRUE, LabMeta = FALSE, FieldObs = FALSE){
  require(httr)
  auth = getauth()
  emptysmalldf = data.frame(Timestamp = character(), Value = character(), stringsAsFactors = FALSE)
  emptybigdf = data.frame(Site = character(), LocationName = character(), Timestamp = character(), Value = character(), Quality = character(), Approval = character(), Qualifiers = character(), Parameter = character(), Unit = character(), Comments = character(), stringsAsFactors = FALSE)
  if (nchar(start) > 0){
    start = paste(start,"+12:00",sep="")
  }
  if (nchar(end) > 0){
    end = paste(end,"+12:00",sep="")
  }
  locationid = strsplit(dataid,'@')[[1]][2]
  label = strsplit((strsplit(dataid,'@')[[1]][1]),".",fixed = TRUE)[[1]][2]
  parameter = strsplit((strsplit(dataid,'@')[[1]][1]),".",fixed = TRUE)[[1]][1]
  if (label == 'Field Visits'){
    sets = datasets(locationid)
    sets = sets[sets$DataId == dataid,]
    if (nrow(sets) == 0){
      print(paste("Requested DataId does not exist - Please review your DataId -",dataid))
      if (MetadataExport == TRUE){
        dater = emptybigdf
      }
      else{
        dater = emptysmalldf
      }
    }
    else{
      dater = getfieldvisitdata(parameter,locationid, start = start, end = end)}
  }
  else{
    sets = datasets(locationid)
    if (nrow(sets) == 0){
      print(paste("Location either has no datasets or does not exist -- Please review your DataId -",dataid))
      if (MetadataExport == TRUE){
        dater = emptybigdf
      }
      else{
        dater = emptysmalldf
      }
    }
    else{
      uniqueid = sets$UniqueId[sets$DataId == dataid]
      if (length(uniqueid) == 0){
        print(paste("Requested DataId does not exist - Please review your DataId -",dataid))
        if (MetadataExport == TRUE){
          dater = emptybigdf
        }
        else{
          dater = emptysmalldf
        }
      }
      else{
        url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetTimeSeriesRawData'
        query = list(TimeSeriesUniqueId = uniqueid, QueryFrom = start, QueryTo = end)
        dater = GET(url_call, query = query)
        dater = fromJSON(content(dater,"text"))
        grades = dater$Grades
        approvals = dater$Approvals
        qualifiers = dater$Qualifiers
        locationid = dater$LocationIdentifier
        parameter = dater$Parameter
        units = dater$Unit
        dater = dater$Points
        if (is.data.frame(dater)){
          dater$Value = dater$Value$Numeric
          dater$Timestamp = as.POSIXct(strptime(paste(substr(dater$Timestamp,1,10),substr(dater$Timestamp,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
          if (MetadataExport == TRUE){
            dater = appendjsonmetadata(dater, grades,'Quality','GradeCode')
            dater = appendjsonmetadata(dater, approvals, 'Approval', 'LevelDescription')
            dater = appendjsonmetadataqualifiers(dater, qualifiers, 'Qualifiers', 'Identifier')
            location_name = getlocationdescriptions(locationid)
            dater$Parameter = parameter
            dater$Unit = units
            dater$Site = locationid
            dater$LocationName = location_name
            dater <- dater[c("Site", "LocationName", "Timestamp", "Value", "Quality", "Approval", "Qualifiers", "Parameter", "Unit")]
          }
          if (LabMeta == TRUE){
            if (!"laberrorlog" %in% ls(envir = .GlobalEnv)){
              laberrorlog <<- get_lab_error_log()}
            locresultanalysis = resultanalysis(locationid)
            locresultanalysis$simple_sampled_datetime = format(locresultanalysis$sampled_datetime,'%Y-%m-%d %H:%M')
            locresultanalysis$Lab = unlist(lapply(locresultanalysis$ANALYSIS, FUN = analysis_to_lab))
            if (nrow(locresultanalysis) < 1){
              locresultanalysis$Lab = character(0)
            }
            dater$simple_sampled_datetime = format(dater$Timestamp,'%Y-%m-%d %H:%M')
            dater = merge(dater,locresultanalysis,by.x = c('simple_sampled_datetime','Parameter','Value'), by.y = c('simple_sampled_datetime','Identifier','ENTRY'), all.x=TRUE)
            dater = merge(dater,laberrorlog,by.x = c('Timestamp','Site'), by.y = c('Date.Time','AquariusID'), all.x=TRUE)
            if (FieldObs == TRUE){
              dater <- dater[c("Site", "LocationName", "Timestamp", "Value", "Quality", "Approval", "Qualifiers", "Parameter", "Unit", "Sample_Number", "Project_Id", "C_LDL", "C_UDL", "C_METHOD_REFERENCE","COLLECTION_METHOD","Lab","Error","Comment","ACTION","Samp_Comment","Cloud_Cover", "Num_Swimmers", "Tide_Cycle", "Tide", "Wind_Direction", "Wind_Speed", "Weather_Today", "Weather_Yest", "Surface_Cond", "Bore_Type", "Bore_Number", "Bore_Collect_Method", "Bore_Sampled_From", "Bore_Pump_Rate", "Bore_Pump_Duration", "Bore_Protocol", "Bore_Probe", "Odour")]
            }
            else{
              dater <- dater[c("Site", "LocationName", "Timestamp", "Value", "Quality", "Approval", "Qualifiers", "Parameter", "Unit", "Sample_Number", "Project_Id", "C_LDL", "C_UDL", "C_METHOD_REFERENCE","COLLECTION_METHOD","Lab","Error","Comment","ACTION")]}
            colnames(dater)[colnames(dater)=="C_METHOD_REFERENCE"] <- "Method"
            colnames(dater)[colnames(dater)=="COLLECTION_METHOD"] <- "CollectionMethod"
            colnames(dater)[colnames(dater)=="C_LDL"] <- "LowerDetectionLimit"
            colnames(dater)[colnames(dater)=="C_UDL"] <- "UpperDetectionLimit"}
        }
        else{
          print(paste(dataid,' has no data for requested period', sep = ''))
          if (MetadataExport == TRUE){
            dater = emptybigdf
          }
          else{
            dater = emptysmalldf
          }
        }
      }
    }
    colnames(dater)[colnames(dater)=="Timestamp"] <- "Time"}
  return (dater)
}

getdatajson <- function(dataid, start = '', end = ''){
  auth = getauth()
  if (nchar(start) > 0){
    start = paste(start,"+12:00",sep="")
  }
  if (nchar(end) > 0){
    end = paste(end,"+12:00",sep="")
  }
  locationid = strsplit(dataid,'@')[[1]][2]
  sets = datasets(locationid)
  uniqueid = sets$UniqueId[sets$DataId == dataid]
  url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetTimeSeriesCorrectedData'
  query = list(TimeSeriesUniqueId = uniqueid, QueryFrom = start, QueryTo = end)
  locations = GET(url_call, query = query)
  locations = fromJSON(content(locations,"text"))
  return (locations)
}

getfieldvisitdata <- function (parameterid, locationid, start = '', end = '') {
  if(!require(hash)){
    install.packages("hash")
    require(hash)
  }
  else{
    require(hash)}
  require(httr)
  auth = getauth()
  url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetFieldVisitDataByLocation?LocationIdentifier'
  query = list(LocationIdentifier = locationid)
  fieldvisits = GET(url_call, query = query)
  fieldvisits = fromJSON(content(fieldvisits,"text"))
  fieldvisits = fieldvisits$FieldVisitData
  parameter_config = getparameters()
  approvals = c()
  grades = c()
  values = c()
  times = c()
  comments = c()
  readingtypes = c()

  mapping = hash("River X Area" = "Area","River X Width" = "Width","Water Velocity" = "VelocityAverage","Stage" = "MeanGageHeight","Discharge" = "Discharge")
  for (i in 1:nrow(fieldvisits)) {
    approval = c(fieldvisits[i,]$Approval$LevelDescription)
    #commenty = c(fieldvisits[i,]$Remarks)
    if (!is.null(fieldvisits[i,]$DischargeActivities[[1]]) & parameterid %in% names(mapping)){
      field = fieldvisits[i,]$DischargeActivities
      for (t in 1:nrow(field[[1]]$DischargeSummary)) {
        if ("MeasurementGrade" %in% names(field[[1]]$DischargeSummary)){
          grade = c(field[[1]]$DischargeSummary$MeasurementGrade[[t]])
        }
        else{
          grade = c("")
        }
        if (parameterid %in% c('River X Area','River X Width','Water Velocity')){
          other = field[[1]]$PointVelocityDischargeActivities[[t]]
          if (mapping[[parameterid]] %in% names(other)){
            value = c(other[[mapping[[parameterid]]]][['Numeric']])
            now = c(field[[1]]$DischargeSummary$MeasurementTime[[t]])
            if (!is.null(value)){
              values = c(values, value)
              times = c(times, now)
              approvals = c(approvals, approval)
              grades = c(grades, grade)}}
        }
        else{
          value = c(field[[1]][['DischargeSummary']][[mapping[[parameterid]]]][['Numeric']][[t]])
          now = c(field[[1]]$DischargeSummary$MeasurementTime[[t]])
          commenty = c(field[[1]]$DischargeSummary$Comments[[t]])
          readingtype = "MeanGageHeight"
          if (!is.null(value)){
            values = c(values, value)
            times = c(times, now)
            approvals = c(approvals, approval)
            comments = c(comments, commenty)
            grades = c(grades, grade)
            readingtypes = c(readingtypes, readingtype)}
        }
      }}
    if (!is.null(fieldvisits[i,]$InspectionActivity$Readings)){
      inspection = fieldvisits[i,]$InspectionActivity$Readings[[1]]
      inspection = inspection[inspection$Publish == TRUE,]
      inspection = inspection[inspection$Parameter == parameterid,]
      field = fieldvisits[i,]$InspectionActivity$Readings
      rownames(inspection) = NULL
      if (length(inspection) > 0){
        if (nrow(inspection) > 0){
          for(k in 1:nrow(inspection)){
            value = inspection[['Value']][['Numeric']][k]
            now = inspection[['Time']][k]

            if ("Comments" %in% names(field[[1]])){
              commenty = inspection[['Comments']][k]
            }
            else{
              commenty = c("")
            }
            readingtype = inspection[['ReadingType']][k]
            if (!is.null(value)){
              values = c(values, value)
              times = c(times, now)
              approvals = c(approvals, approval)
              grades = c(grades, c(''))
              comments = c(comments, commenty)
              readingtypes = c(readingtypes, readingtype)}

          }
        }
      }
    }

  }
  final = data.frame(Datetime_Time = times, Value = values, Approval = approvals, Quality = grades, ReadingType = readingtypes, Comments = comments)
  final$Time = as.POSIXct(strptime(paste(substr(final$Datetime_Time,1,10),substr(final$Datetime_Time,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
  final$Site = locationid
  locationmeta = searchlocationid(locationid)
  final$LocationName = locationmeta$LocationName
  final$Parameter = parameterid
  parameter_unit = parameter_config[parameter_config$Identifier == parameterid,]$UnitIdentifier
  final$Unit = parameter_unit
  final$Qualifiers = ''
  final = final[c("Site", "LocationName", "Time", "Value", "Quality", "Approval", "Qualifiers", "Parameter", "Unit", "ReadingType", "Comments")]
  final = final[order(final$Time),]
  if (start != ''){
    final = final[final$Time >= start,]
  }
  if (end != ''){
    final = final[final$Time <= end,]
  }
  rownames(final) = NULL
  return(final)
}

get_fieldvisit_datasets <- function(){
  datasets = read.csv('\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\FieldVisit Exporter\\all_field_visit_datasets.csv', encoding="ISO-8859-1")
  return(datasets)}

AQMultiExtract <- function (sitelist, param, start = "", end = "", site_merge = TRUE, AppendFlow = FALSE, Qualifiers = '')
{
  require(reshape2)
  require(httr)
  require(RODBC)
  #turn off warnings
  options(warn = -1)

  #set up steps for progress bar
  stepi = 0
  iterations <- length(sitelist) * length(param)
  pb = txtProgressBar(min = 0, max = iterations, style = 3)

  #table of cross over sites

  # cross_over <- data.frame(current = c("MK307635","BR518499","BQ766632","MI036134","FO620177"),
  #                          old = c("MK283728","BR587418","BQ711622","LI953392","FO591211"),
  #                          change = c(as.Date("2014-10-01"),as.Date("2015-10-01"),as.Date("2017-07-01"),as.Date("2016-07-01"),as.Date("2011-10-01")))

  #cross_over <-read.csv("https://objective.envbop.net/id:A4030732/document/versions/published")

  data("Cross_Over_Sites")
  cross_over <- Cross_Over_Sites
  #cross_over <-read.csv("V:/Applications/James/R Course/cross_over_data.csv")
 # cross_over <-GET("https://objective.envbop.net/id:A4030732/document/versions/published", authenticate(":",":",type="gssnegotiate"))
  #writeBin(content(cross_over, "raw"), "cross_over_data.csv")
  #cross_over <- read.csv("cross_over_data.csv")
  #explicit convertion to data frame


  cross_over <-cross_over[,c("current","old","change")]
  cross_over$change <- as.POSIXct(strptime(cross_over$change,format = "%d/%m/%Y"),tz="")

  #set up final dataframe
  fdf <- data.frame(RangeNumber = integer(), Time = character(),
                    Value = numeric(), Quality = integer(), Interpolation = integer(),
                    Approval = integer(), Site = character(), Parameter = character())

  #list of all parameters
  pdf <- getparameters()

  #loop through each site
  for (i in 1:length(sitelist)) {

    if (AppendFlow == TRUE){
      param = c(param,c('Discharge'))
      discharge_metadata = read.csv("\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\SampleTime Discharge\\SampleTime_Discharge_Metadata.csv")
      discharge_metadata$SamplingTime = as.POSIXct(strptime(paste(substr(discharge_metadata$SamplingTime,1,10),substr(discharge_metadata$SamplingTime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
      discharge_metadata$DischargeTime = as.POSIXct(strptime(paste(substr(discharge_metadata$DischargeTime,1,10),substr(discharge_metadata$DischargeTime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
    }

    #if site merge is turned on AND time period is either not specified or crosses over the site change, do this:
    if(site_merge == TRUE & sitelist[i] %in% cross_over$current == TRUE & cross_over[match(sitelist[i],cross_over$current),"change"] > as.Date(start, "%Y-%m-%d")|
       site_merge == TRUE & sitelist[i] %in% cross_over$old == TRUE & cross_over[match(sitelist[i],cross_over$old),"change"] > as.Date(start, "%Y-%m-%d")|
       site_merge == TRUE & sitelist[i] %in% cross_over$current == TRUE & start == ""|
       site_merge == TRUE & sitelist[i] %in% cross_over$old == TRUE & start == ""){

      if(sitelist[i] %in% cross_over$current == TRUE){
        #pull out site specific information from the cross over table
        match_data <- cross_over[match(sitelist[i],cross_over$current),]

      }else{
        match_data <- cross_over[match(sitelist[i],cross_over$old),]
      }

      #extract site name from aquarius
      location_metadata <- searchlocationid(match_data[1,1])
      print(paste0("SiteID ", match_data[1,1]," changed location during timespan.  Dataset merged with ",match_data[1,2]," as of ",match_data[1,3],"."))
      loc_name = location_metadata$LocationName
      if (is.character(loc_name)){

        #loop through parameters
        for (x in 1:length(param)) {


          #for progress bar
          stepi = stepi + 1

          #parameter id's and units
          param_id <- param[x]
          param_unit <-pdf$UnitIdentifier[pdf$Identifier == param[x]]

          #Total Cyanobacteria is stored under .FieldResult instead of .LabResult
          if (param_id %in% c("Total Cyanobacteria","EPT Taxa Percent","MCI Score","ASPM","QMCI","TaxaRich","Potentially Toxic Cyanobacteria")) {

            #data id for new site
            datid <- paste(param_id, ".FieldResult@", match_data[1,1], sep = "")

            #data id for old site
            datid_old <- paste(param_id, ".FieldResult@", match_data[1,2], sep = "")

          }
          else if (param_id == 'Discharge') {

            #data id for new site
            datid <- paste(param_id, ".SampleTime@", match_data[1,1], sep = "")

            #data id for old site
            datid_old <- paste(param_id, ".SampleTime@", match_data[1,2], sep = "")
          }
          else {

            #data id for new site
            datid <- paste(param_id, ".LabResult@", match_data[1,1], sep = "")

            #data id for old site
            datid_old <- paste(param_id, ".LabResult@", match_data[1,2],sep = "")
          }

          #if date is not defined, extract all data using new and old data ids and then subset to change over based on date in cross over table
          if(start == ""){
            df_new <- data.frame()
            df_new <- getdata(datid)
            df_new <- df_new[as.Date(df_new$Time) > as.Date(match_data[1,3]),]
            df_old <- data.frame()
            df_old <- getdata(datid_old)
            df_old <- df_old[as.Date(df_old$Time) < as.Date(match_data[1,3]),]
          }
          #if dates are defined, use these to pass to getdata function
          else{
            df_new <- data.frame()
            df_new <- getdata(datid,start = match_data[1,3],end=end)
            df_old <- data.frame()
            df_old <- getdata(datid_old,start=start, end = match_data[1,3])
          }

          #combine old and new dfs, add site ID's, add parameter names,add location name
          df <- rbind(df_old, df_new)
          if (nrow(df) > 0){
            df$Site <- match_data[1,1]
            parameter_string = paste(param[x], " (", param_unit,")", sep = "")
            df$Parameter <- parameter_string
            df$LocationName <- loc_name

            #bind to final dataframe
            fdf <- rbind(fdf, df)}

          #progress progress bar one iteration
          setTxtProgressBar(pb, stepi)

        }}}

    else { #if site is not on list, merge is not turned on, or dates do not span crossover

      #location name string
      location_metadata <- searchlocationid(sitelist[i])
      loc_name = location_metadata$LocationName
      if (is.character(loc_name)){

        #loop through parameters
        for (x in 1:length(param)) {
          #iteration
          stepi = stepi + 1
          #parameter information
          param_id <- param[x]
          param_unit <-pdf$UnitIdentifier[pdf$Identifier == param[x]]
          #if Total Cyanobacteria do this
          if (param_id %in% c("Total Cyanobacteria","EPT Taxa Percent","MCI Score","ASPM","QMCI","TaxaRich","Potentially Toxic Cyanobacteria")) {
            datid <- paste(param_id, ".FieldResult@", sitelist[i],
                           sep = "")
          }
          else if (param_id == 'Discharge') {
            datid <- paste(param_id, ".SampleTime@",
                           sitelist[i], sep = "")
          }
          else { #otherwise do this
            datid <- paste(param_id, ".LabResult@",
                           sitelist[i], sep = "")
          }

          #extract data
          df <- getdata(datid, start = start, end = end)

          #niwa wq data workaround
          if (sitelist[i] %in% c('GJ662805', 'JM102399', 'IG265664', 'IG691428', 'JL350292', 'QJ471191', 'QM756918')){
            location_datasets = datasets(sitelist[i])
            location_datasets = location_datasets[location_datasets$Parameter == param_id & location_datasets$Label == 'NIWA',]
            if (nrow(location_datasets) > 0){
              print(paste("NIWA data found - appending dataset -",paste(param_id,".NIWA@", sitelist[i], sep = "")))
              niwa_data = getdata(paste(param_id,".NIWA@", sitelist[i], sep = ""), start = start, end = end)
              #remove all results from labdf that have a time in niwa data - basically take preference of niwa data
              df = df[!df$Time %in% niwa_data$Time,]
              df = rbind(df, niwa_data)
            }
          }

          if (nrow(df) > 0){

            #add site IDs
            df$Site <- sitelist[i]

            #add parameter name and unit
            parameter_string = paste(param[x], " (", param_unit, ")", sep = "")
            df$Parameter <- parameter_string

            #add location name
            df$LocationName <- loc_name

            #bind to final df
            fdf <- rbind(fdf, df)}

          #progress bar update
          setTxtProgressBar(pb, stepi)
        }
      }
    }# end of cross-over loop
  }#end of site loop


  #finally, cast the fdf object into columns
  if (nrow(fdf) > 0){
    if (nchar(Qualifiers) > 0){
      fdf <- fdf[grep(Qualifiers,fdf$Qualifiers),]
    }
    fdf <- dcast(fdf, Site + LocationName + Time ~ Parameter,value.var = "Value",fun.aggregate = mean)}
  else{
    print('No data for AQMultiExtract request found')
    fdf = data.frame()
  }

  if (AppendFlow == TRUE){
    fdf = merge(x = fdf, y = discharge_metadata, by.x = c("Site","Time"), by.y = c("SamplingLocationId","SamplingTime"), all.x = TRUE)
    fdf = fdf[,c(names(fdf)[!names(fdf) %in% c('Discharge (m^3/s)','DischargeLocation','DischargeTime','DischargeType','DischargeApproval')],c('Discharge (m^3/s)','DischargeLocation','DischargeTime','DischargeType','DischargeApproval'))]}

  #return fdf object
  return(fdf)

  #turn warnings back on
  options(warn = 0)}

AQMultiExtractFlat <- function (sitelist, param, start = "", end = "", site_merge = TRUE,  FlatView = TRUE,AppendFlow = FALSE)

{

  require(reshape2)
  require(httr)
  require(RODBC)

  #turn off warnings
  options(warn = -1)

  #set up steps for progress bar
  stepi = 0
  iterations <- length(sitelist) * length(param)
  pb = txtProgressBar(min = 0, max = iterations, style = 3)

  if (FlatView == TRUE){
    LabMeta = TRUE
    FieldObs = TRUE
  }
  else{
    LabMeta = FALSE
    FieldObs = FALSE
  }

  #table of cross over sites

  # cross_over <- data.frame(current = c("MK307635","BR518499","BQ766632","MI036134","FO620177"),
  #                          old = c("MK283728","BR587418","BQ711622","LI953392","FO591211"),
  #                          change = c(as.Date("2014-10-01"),as.Date("2015-10-01"),as.Date("2017-07-01"),as.Date("2016-07-01"),as.Date("2011-10-01")))

  #cross_over <-read.csv("https://objective.envbop.net/id:A4030732/document/versions/published")

  data("Cross_Over_Sites")
  cross_over <- Cross_Over_Sites

  #cross_over <-GET("https://objective.envbop.net/id:A4030732/document/versions/published", authenticate(":",":",type="gssnegotiate"))
  #writeBin(content(cross_over, "raw"), "cross_over_data.csv")
  #cross_over <- read.csv("cross_over_data.csv")
  cross_over <-cross_over[,c("current","old","change")]
  cross_over$change <- as.POSIXct(strptime(cross_over$change,format = "%d/%m/%Y"),tz="")

  #set up final dataframe
  fdf <- data.frame(RangeNumber = integer(), Time = character(),
                    Value = numeric(), Quality = integer(), Interpolation = integer(),
                    Approval = integer(), Site = character(), Parameter = character())

  #list of all parameters
  pdf <- getparameters()

  #loop through each site
  for (i in 1:length(sitelist)) {

    if (AppendFlow == TRUE){
      param = c(param,c('Discharge'))
      final_flow = data.frame()
      #TODO - remove _Wait after successful run of sampletime_discharge
      discharge_metadata = read.csv("\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\SampleTime Discharge\\SampleTime_Discharge_Metadata.csv")  ### PROB NEEDS TO CHANGE??
      discharge_metadata$SamplingTime = as.POSIXct(strptime(paste(substr(discharge_metadata$SamplingTime,1,10),substr(discharge_metadata$SamplingTime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
      discharge_metadata$DischargeTime = as.POSIXct(strptime(paste(substr(discharge_metadata$DischargeTime,1,10),substr(discharge_metadata$DischargeTime,12,19)), "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+12")
    }

    #if site merge is turned on AND time period is either not specified or crosses over the site change, do this:
    if(site_merge == TRUE & sitelist[i] %in% cross_over$current == TRUE & cross_over[match(sitelist[i],cross_over$current),"change"] > as.Date(start, "%Y-%m-%d")|
       site_merge == TRUE & sitelist[i] %in% cross_over$old == TRUE & cross_over[match(sitelist[i],cross_over$old),"change"] > as.Date(start, "%Y-%m-%d")|
       site_merge == TRUE & sitelist[i] %in% cross_over$current == TRUE & start == ""|
       site_merge == TRUE & sitelist[i] %in% cross_over$old == TRUE & start == ""){

      if(sitelist[i] %in% cross_over$current == TRUE){
        #pull out site specific information from the cross over table
        match_data <- cross_over[match(sitelist[i],cross_over$current),]

      }else{
        match_data <- cross_over[match(sitelist[i],cross_over$old),]
      }

      #extract site name from aquarius
      location_metadata <- searchlocationid(match_data[1,1])
      print(paste0("SiteID ", match_data[1,1]," changed location during timespan.  Dataset merged with ",match_data[1,2]," as of ",match_data[1,3],"."))
      loc_name = location_metadata$LocationName
      if (is.character(loc_name)){

        #loop through parameters
        for (x in 1:length(param)) {


          #for progress bar
          stepi = stepi + 1

          #parameter id's and units
          param_id <- param[x]
          param_unit <-pdf$UnitIdentifier[pdf$Identifier == param[x]]

          #Total Cyanobacteria is stored under .FieldResult instead of .LabResult
          if (param_id %in% c("Total Cyanobacteria","EPT Taxa Percent","MCI Score","ASPM","QMCI","TaxaRich","Potentially Toxic Cyanobacteria")) {

            #data id for new site
            datid <- paste(param_id, ".FieldResult@", match_data[1,1], sep = "")

            #data id for old site
            datid_old <- paste(param_id, ".FieldResult@", match_data[1,2], sep = "")

          }
          else if (param_id == 'Discharge') {

            #data id for new site
            datid <- paste(param_id, ".SampleTime@", match_data[1,1], sep = "")

            #data id for old site
            datid_old <- paste(param_id, ".SampleTime@", match_data[1,2], sep = "")
          }
          else {

            #data id for new site
            datid <- paste(param_id, ".LabResult@", match_data[1,1], sep = "")

            #data id for old site
            datid_old <- paste(param_id, ".LabResult@", match_data[1,2],sep = "")
          }

          #if date is not defined, extract all data using new and old data ids and then subset to change over based on date in cross over table
          if(start == ""){
            df_new <- data.frame()
            df_new <- getdata(datid, LabMeta = LabMeta, FieldObs = FieldObs)
            df_new <- df_new[as.Date(df_new$Time) > as.Date(match_data[1,3]),]
            df_old <- data.frame()
            df_old <- getdata(datid_old, LabMeta = LabMeta, FieldObs = FieldObs)
            df_old <- df_old[as.Date(df_old$Time) < as.Date(match_data[1,3]),]
          }
          #if dates are defined, use these to pass to getdata function
          else{
            df_new <- data.frame()
            df_new <- getdata(datid,start = match_data[1,3],end=end, LabMeta = LabMeta, FieldObs = FieldObs)
            df_old <- data.frame()
            df_old <- getdata(datid_old,start=start, end = match_data[1,3], LabMeta = LabMeta, FieldObs = FieldObs)
          }

          #combine old and new dfs, add site ID's, add parameter names,add location name
          df <- rbind(df_old, df_new)
          if (nrow(df) > 0){
            df$Site <- match_data[1,1]
            parameter_string = paste(param[x], " (", param_unit,")", sep = "")
            df$Parameter <- parameter_string
            df$LocationName <- loc_name

            if (param[x] == 'Discharge' & FlatView == TRUE){
              flow <- merge(x = df, y = discharge_metadata, by.x = c("Site","Time"), by.y = c("SamplingLocationId","SamplingTime"), all.x = TRUE)
              flow <- flow[c("Site", "Time", "Value", "DischargeLocation", "DischargeTime", "DischargeType")]
              colnames(flow)[colnames(flow)=="Value"] <- "Discharge (m^3/s)"
              colnames(flow)[colnames(flow)=="Site"] <- "SamplingLocationId"
              colnames(flow)[colnames(flow)=="Time"] <- "SamplingTime"
              final_flow <- rbind(final_flow,flow)}

            else{
              #bind to final dataframe
              fdf <- rbind(fdf, df)}
          }

          #progress progress bar one iteration
          setTxtProgressBar(pb, stepi)

        }}}

    else { #if site is not on list, merge is not turned on, or dates do not span crossover

      #location name string
      location_metadata <- searchlocationid(sitelist[i])
      loc_name = location_metadata$LocationName
      if (is.character(loc_name)){

        #loop through parameters
        for (x in 1:length(param)) {
          #iteration
          stepi = stepi + 1
          #parameter information
          param_id <- param[x]
          param_unit <-pdf$UnitIdentifier[pdf$Identifier == param[x]]
          #if Total Cyanobacteria do this
          if (param_id %in% c("Total Cyanobacteria","EPT Taxa Percent","MCI Score","ASPM","QMCI","TaxaRich","Potentially Toxic Cyanobacteria")) {
            datid <- paste(param_id, ".FieldResult@", sitelist[i],
                           sep = "")
          }
          else if (param_id == 'Discharge') {
            datid <- paste(param_id, ".SampleTime@",
                           sitelist[i], sep = "")
          }
          else { #otherwise do this
            datid <- paste(param_id, ".LabResult@",
                           sitelist[i], sep = "")
          }

          #extract data
          df <- getdata(datid, start = start, end = end, LabMeta = LabMeta, FieldObs = FieldObs)

          #niwa wq data workaround
          if (sitelist[i] %in% c('GJ662805', 'JM102399', 'IG265664', 'IG691428', 'JL350292', 'QJ471191', 'QM756918')){
            location_datasets = datasets(sitelist[i])
            location_datasets = location_datasets[location_datasets$Parameter == param_id & location_datasets$Label == 'NIWA',]
            if (nrow(location_datasets) > 0){
              print(paste("NIWA data found - appending dataset -",paste(param_id,".NIWA@", sitelist[i], sep = "")))
              niwa_data = getdata(paste(param_id,".NIWA@", sitelist[i], sep = ""), start = start, end = end, LabMeta = LabMeta, FieldObs = FieldObs)
              #remove all results from labdf that have a time in niwa data - basically take preference of niwa data
              df = df[!df$Time %in% niwa_data$Time,]
              df = rbind(df, niwa_data)
            }
          }

          if (nrow(df) > 0){

            #add site IDs
            df$Site <- sitelist[i]

            #add parameter name and unit
            parameter_string = paste(param[x], " (", param_unit, ")", sep = "")
            df$Parameter <- parameter_string

            #add location name
            df$LocationName <- loc_name

            if (param[x] == 'Discharge' & FlatView == TRUE){
              flow <- merge(x = df, y = discharge_metadata, by.x = c("Site","Time"), by.y = c("SamplingLocationId","SamplingTime"), all.x = TRUE)
              flow <- flow[c("Site", "Time", "Value", "DischargeLocation", "DischargeTime", "DischargeType")]
              colnames(flow)[colnames(flow)=="Value"] <- "Discharge (m^3/s)"
              colnames(flow)[colnames(flow)=="Site"] <- "SamplingLocationId"
              colnames(flow)[colnames(flow)=="Time"] <- "SamplingTime"
              final_flow <- rbind(final_flow,flow)}
            else{
              #bind to final df
              fdf <- rbind(fdf, df)}
          }

          #progress bar update
          setTxtProgressBar(pb, stepi)
        }
      }
    }# end of cross-over loop
  }#end of site loop


  #finally, cast the fdf object into columns
  if (nrow(fdf) > 0){
    if (FlatView != TRUE){
      fdf <- dcast(fdf, Site + LocationName + Time ~ Parameter,value.var = "Value")}
    if (AppendFlow == TRUE){
      if (FlatView != TRUE){
        if ("Discharge (m^3/s)" %in% colnames(fdf)){
          fdf = merge(x = fdf, y = discharge_metadata, by.x = c("Site","Time"), by.y = c("SamplingLocationId","SamplingTime"), all.x = TRUE)
          fdf = fdf[,c(names(fdf)[!names(fdf) %in% c('Discharge (m^3/s)','DischargeLocation','DischargeTime','DischargeType')],c('Discharge (m^3/s)','DischargeLocation','DischargeTime','DischargeType'))]}
        else{
          print('No SampleTime.Discharge values for locations/times selected')
        }
        }
      else{
        if (nrow(final_flow) > 0){
          fdf = merge(x = fdf, y = final_flow, by.x = c("Site","Time"), by.y = c("SamplingLocationId","SamplingTime"), all.x = TRUE)
          fdf = fdf[,c(names(fdf)[!names(fdf) %in% c('Discharge (m^3/s)','DischargeLocation','DischargeTime','DischargeType')],c('Discharge (m^3/s)','DischargeLocation','DischargeTime','DischargeType'))]}
        else{
          print('No SampleTime.Discharge values for locations/times selected')
        }
      }
      }
  }
  else{
    print('No data for AQMultiExtract request found')
    fdf = data.frame()
  }

  #return fdf object
  return(fdf)

  #turn warnings back on
  options(warn = 0)}


AQOpenairExtract <- function(locationid, freq , start = '', end = ''){
  if(!require(reshape2)){
    install.packages("reshape2")
    require(reshape2)
  }
  else{
    require(reshape2)
  }
  fdf <- data.frame(RangeNumber=integer(),Time=character(),Value=numeric(),
                    Quality=integer(),Interpolation=integer(),Approval=integer(),
                    Parameter=character())
  dater = datasets(locationid)
  dater$spilter = sapply(dater$DataId, function (x) strsplit(as.character(x),"@")[[1]][1])
  windparams = c('Wind Vel', 'Wind Dir', 'SO2', 'H2S', 'TSP', 'Air Temp', 'Rel Humidity', 'Precip Total', 'HF')
  columnnames = c('ws','wd','so2','h2s','tsp','temp','rh','rain','hf')
  dataframes = dater$spilter
  no_label <- c()
  for(x in 1:length(windparams)){
    if (is.element(paste(windparams[x],".",freq, sep=''),dataframes)) {
      print(paste(windparams[x],".",freq,"@",locationid, sep=''))
      df <- getdata(paste(windparams[x],".",freq,"@",locationid, sep=''),start = start, end = end)
      df$Parameter <- columnnames[x]
      fdf <- rbind(fdf,df)
    } else{
      no_label <-c(no_label,columnnames[x])
    }
  }
  output = dcast(fdf, Time ~ Parameter,value.var = "Value")
  if (length(no_label) > 0){
    for (x in 1:length(no_label)){
      output[[no_label[x]]] <- NA
    }
  }
  lister = c("Time", columnnames)
  output <- output[, lister]
  colnames(output)[1] <- c("date")
  return(output)
}

#Function to append flow to multiexract.  WQ data must be in the format: SITEID, Name, TIME, Parameter1:x.  Flow data must be in the format: SiteID, Site Name, Time, Flow.

AQAppendFlow <- function(wqdata, flowdata,bracket){
  names(flowdata) <- c("ID","Name","Time", "Discharge")
  wqdata[,2] <- as.factor(wqdata[,2])
  wqdata$Flow <- NA
  lvls <- levels(wqdata[,2])
  output <- data.frame(setNames(replicate(length(colnames(wqdata)),numeric(0),simplify = F),colnames(wqdata)))

  for(i in 1:length(lvls)){
    currsite <- lvls[i]
    sitedf <-wqdata[wqdata[,2] == currsite ,]
    flowdf <- flowdata[flowdata[,2] == currsite, ]
    if(length(sitedf[,1]) > 0 & length(flowdf[,1]) > 0){
      for(x in 1:length(sitedf$Time)){
        day <- format(as.Date(sitedf$Time[x],tz = "Etc/GMT+12"), "%Y-%m-%d")
        flowdf$daygap <- difftime(day,flowdf$Time,tz = "Etc/GMT+12",units="days")
        mingap <- which(abs(flowdf$daygap-0)==min(abs(flowdf$daygap-0)))

        if(length(mingap)==1){
          sitedf$Flow[x] <-ifelse(abs(flowdf$daygap[mingap])<bracket,flowdf$Discharge[mingap] ,"N/A")
        }else{
          mingapdf <- as.data.frame(0)
          for(p in 1:length(mingap)){
            mingapdf[p] <- flowdf$Discharge[mingap[p]]
          }
          sitedf$Flow[x] <-ifelse(abs(flowdf$daygap[mingap[1]])<bracket,mean(as.numeric(mingapdf)) ,"N/A")
          rm(mingapdf)
        }
      }

    }else{

      print(paste("Missing Flow data for",currsite))

    }

    if(i == 1){
      output <- sitedf
    }else{
      output <- rbind(output,sitedf)
    }

  }
  output$Flow <- as.numeric(output$Flow)
  return(output)
}

update_aquarius_package <- function() {
  latest_packages = list.files("\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\DataServices\\R\\Packages")
  latest_package = latest_packages[grep("aquarius2018_",latest_packages)]
  install.packages(paste("\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\DataServices\\R\\Packages\\",latest_package, sep = ""), repos = NULL)
  .rs.restartR()
}

update_aquarius_package_local <- function() {
  latest_packages = list.files("C:\\DataServices\\R\\Packages")
  latest_package = latest_packages[grep("aquarius2018_",latest_packages)]
  install.packages(paste("C:\\DataServices\\R\\Packages\\",latest_package, sep = ""), repos = NULL)
  .rs.restartR()
}

## Major refactor -- much more efficient and will append to any dimension df -- SiteName, RecID -- Still TODO

Site_Metadata <- function (data){
  locationids = unique(data[,1])
  eastnorthdf = data.frame()
  for (i in 1:length(locationids)){
    locmeta = searchlocationid(locationids[i])
    easting = ifelse(nchar(locmeta$EASTING)<8, locmeta$EASTING, "NA")
    northing = ifelse(nchar(locmeta$NORTHING)<8, locmeta$NORTHING, "NA")
    locationname = locmeta$LocationName
    recid = locmeta$RECNZREACHID
    eastnorthdf[i,'Easting'] = easting
    eastnorthdf[i,'Northing'] = northing
    eastnorthdf[i,'LocationId'] = locationids[i]
    eastnorthdf[i,'LocationName'] = locationname
    eastnorthdf[i,'RecID'] = recid
  }
  data = merge(data,eastnorthdf, by.x = colnames(data)[1], by.y = "LocationId")
  return(data)
}


FlowtoMeanDaily <- function(Flow, Gap_Fill = FALSE){
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  else{
    require(dplyr)}
  Flow$Day <- cut(Flow$Time, breaks = "day")
  Flow <- Flow %>% group_by(Day) %>% summarise(Value=mean(Value,na.rm = T))
  Flow$Day <- as.POSIXct(Flow$Day)
  if (Gap_Fill == TRUE){
    ts <- seq.POSIXt(min(Flow$Day), max(Flow$Day), by="day")
    ts <- as.Date(ts,format = "%Y-%m-%d %H:%M:%S",tz="Etc/GMT+12")
    ts_df <- data.frame(Day=ts)
    ts_df$Day <- as.character(ts_df$Day)
    Flow$Day <- as.character(Flow$Day)
    Flow <- full_join(ts_df,Flow,by="Day")
    Flow$Value[is.na(Flow$Value)] <- 0}
  colnames(Flow) <- c('Day','Flow')
  return(Flow)
}

FlowtoMeanHourly <- function(Flow, Gap_Fill = FALSE){
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  else{
    require(dplyr)}
  Flow$Hour <- cut(Flow$Time, breaks = "hour")
  Flow <- Flow %>% group_by(Hour) %>% summarise(Value=mean(Value,na.rm = T))
  Flow$Hour <- as.POSIXct(Flow$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+12")
  print(Flow)
  if (Gap_Fill == TRUE){
    ts <- seq.POSIXt(min(Flow$Hour), max(Flow$Hour), by="hour")
    ts <- as.POSIXct(ts,format = "%Y-%m-%d %H:%M:%S",tz="Etc/GMT+12")
    ts_df <- data.frame(Hour=ts)
    ts_df$Hour <- as.character(ts_df$Hour)
    Flow$Hour <- as.character(Flow$Hour)
    Flow <- full_join(ts_df,Flow,by="Hour")
    Flow$Value[is.na(Flow$Value)] <- 0}
  colnames(Flow) <- c('Hour','Flow')
  return(Flow)
}

sample_flowsite_mapping <- function() {
  if(!require(xlsx)){
    install.packages("xlsx")
    require(xlsx)
  }
  else{
    require(xlsx)}
  require(httr)
  #download.file("https://objective.envbop.net/id:A2741215/document/versions/latest", destfile = "\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\R\\configuration_files\\latest_nermn_config_from_objective.xlsx", mode = "wb")
  flow_site_dat <-GET("https://objective.envbop.net/id:A2741215/document/versions/latest", authenticate(":",":",type="gssnegotiate"))
  writeBin(content(flow_site_dat, "raw"), "\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\R\\configuration_files\\latest_nermn_config_from_objective.xlsx")
  #cross_over <- read.csv("cross_over_data.csv")
  dftest <- read.xlsx("\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\Outputs\\R\\configuration_files\\latest_nermn_config_from_objective.xlsx", 1)
  return(dftest)
}

searchparameterlocations <- function(Parameter){
  require(httr)
  auth = getauth()
  url_call = 'http://brcsvapp08:80/AQUARIUS/Publish/v2/GetTimeSeriesDescriptionList'
  query = list(Parameter = Parameter)
  datasets = GET(url_call, query = query)
  datasets = fromJSON(content(datasets,"text"))
  datasets = datasets$TimeSeriesDescriptions
  return (datasets)}

Summarise_15Mins <- function (Data, Method = "Mean", Gap_Fill = FALSE)
{
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  else{
    require(dplyr)}

  Data$Day <- cut(Data$Time, breaks = "15 min")


  if(Method=="Mean"|Method=="mean"|Method=="MEAN"){
    if(Gap_Fill == TRUE){
      print("Data summarised by fifteen minute means.  Gaps filled to complete the dataset.")
      Data_Daily <- Data %>% group_by(Day) %>% summarise(Value=mean(Value,na.rm = T))
      Data_Daily$Day <- as.POSIXct(Data_Daily$Day, "%Y-%m-%d %H:%M:%S",tz="Etc/GMT+12")

      ts <- seq.POSIXt(min(Data_Daily$Day), max(Data_Daily$Day), by="15 min")
      ts <- as.POSIXct(ts,format = "%Y-%m-%d %H:%M:%S",tz="Etc/GMT+12")

      ts_df <- data.frame(Day=ts)
      ts_df$Day <- as.character(ts_df$Day)
      Data_Daily$Day <- as.character(Data_Daily$Day)
      Data_Daily <- full_join(ts_df,Data_Daily,by="Day")
      Data_Daily$Value[is.na(Data_Daily$Value)] <- 0

    }else{
      print("Data summarised by fifteen minute means.")
      Data_Daily <- Data %>% group_by(Day) %>% summarise(Value=mean(Value,na.rm = T))
    }


  }else{
    if(Gap_Fill == TRUE){
      print("Data summarised by fifteen minute totals.  Gaps filled to complete the dataset.")
      Data_Daily <- Data %>% group_by(Day) %>% summarise(Value=sum(Value,na.rm = T))
      Data_Daily$Day <- as.POSIXct(Data_Daily$Day, format = "%Y-%m-%d %H:%M:%S",tz="Etc/GMT+12")

      ts <- seq.POSIXt(min(Data_Daily$Day), max(Data_Daily$Day), by="15 min")
      ts <- as.POSIXct(ts,format = "%Y-%m-%d %H:%M:%S",tz="Etc/GMT+12")

      ts_df <- data.frame(Day=ts)
      ts_df$Day <- as.character(ts_df$Day)
      Data_Daily$Day <- as.character(Data_Daily$Day)
      Data_Daily <- full_join(ts_df,Data_Daily,by="Day")
      Data_Daily$Value[is.na(Data_Daily$Value)] <- 0

    }else{
      print("Data summarised by fifteen minute totals")
      Data_Daily <- Data %>% group_by(Day) %>% summarise(Value=sum(Value,na.rm = T))

    }
  }

  Data_Daily$Day <- as.POSIXct(Data_Daily$Day, tz = "Etc/GMT+12")
  colnames(Data_Daily) <- c("Time", "Value")
  Data_Daily = Data_Daily[, c(1, 2)]
  return(Data_Daily)
}

SumHourlyRainfall <- function(Rainfall, Gap_Fill = FALSE){
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  else{
    require(dplyr)}
  Rainfall$Hour <- cut(Rainfall$Time, breaks = "hour")
  Rainfall <- Rainfall %>% group_by(Hour) %>% summarise(Value=sum(Value,na.rm = T))
  Rainfall$Hour <- as.POSIXct(Rainfall$Hour, tz = "Etc/GMT+12")
  if (Gap_Fill == TRUE){
    ts <- seq.POSIXt(min(Rainfall$Hour), max(Rainfall$Hour), by="hour")
    ts <- as.POSIXct(ts,format = "%Y-%m-%d %H:%M:%S",tz="Etc/GMT+12")
    ts_df <- data.frame(Hour=ts)
    ts_df$Hour <- as.character(ts_df$Hour)
    Rainfall$Hour <- as.character(Rainfall$Hour)
    Rainfall <- full_join(ts_df,Rainfall,by="Hour")
    Rainfall$Value[is.na(Rainfall$Value)] <- 0}
  colnames(Rainfall) <- c('Hour','Rainfall')
  return(Rainfall)
}

SumDailyRainfall <- function(Rainfall, Gap_Fill = FALSE){
  if(!require(dplyr)){
    install.packages("dplyr")
    require(dplyr)
  }
  else{
    require(dplyr)}
  Rainfall$Day <- cut(Rainfall$Time, breaks = "days")
  Rainfall <- Rainfall %>% group_by(Day) %>% summarise(Value=sum(Value,na.rm = T))
  Rainfall$Day <- as.POSIXct(Rainfall$Day, tz = "Etc/GMT+12")
  if (Gap_Fill == TRUE){
    ts <- seq.POSIXt(min(Rainfall$Day), max(Rainfall$Day), by="day")
    ts <- as.Date(ts,format = "%Y-%m-%d %H:%M:%S",tz="Etc/GMT+12")
    ts_df <- data.frame(Day=ts)
    ts_df$Day <- as.character(ts_df$Day)
    Rainfall$Day <- as.character(Rainfall$Day)
    Rainfall <- full_join(ts_df,Rainfall,by="Day")
    Rainfall$Value[is.na(Rainfall$Value)] <- 0}
  colnames(Rainfall) <- c('Day','Rainfall')
  return(Rainfall)
}

ExtremeValueAnalysis <- function(dataid){
  ip <- as.data.frame(installed.packages())$Package
  required_packages = c("shiny","plotly","DT","extRemes","hydroTSM","mgcv")
  for (i in 1:length(required_packages)){
    pack = required_packages[i]
    if (pack %in% ip) {
      require(required_packages[i], character.only = TRUE)
    }
    else{
      stop(paste("Please install the package ",required_packages[i]))
    }
  }
  config = list(
    server = "brcsvapp08", username = "APIaccess", password = "itsdatatime",    # AQTS credentials for your server
    timeSeriesName = dataid, # The time-series to analyze
    eventPeriodStartDay = Sys.Date() - 30, eventPeriodEndDay = Sys.Date()) # The period to analyze


  # Define UI for random distribution app ----
  ui <- fluidPage(

    # App title ----
    titlePanel("Extreme Value Analysis (EVA)"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(

        # Input: Select the distribution type ----
        selectInput(inputId = "dist",
                    label = "Distribution type:",
                    choices = c("Generalized extreme value (GEV)" = "GEV",
                                "Gumbel" = "Gumbel",
                                "Exponential" = "Exponential"),
                    selected = "Gumbel"),


        # Input: Select estimation method ----
        selectInput(inputId = "method",
                    label = "Estimation Methods:",
                    choices = c("MLE" = "MLE",
                                "GMLE" = "GMLE"),
                    selected = "MLE"),

        # br() element to introduce extra vertical spacing ----
        br(),

        textInput(inputId = "timeSeriesName",
                  label = "AQ time-series Name:",
                  value = config$timeSeriesName),
        dateRangeInput("daterange", "Time-series Range:",
                       start  = config$eventPeriodStartDay,
                       end    = config$eventPeriodEndDay),
        submitButton("Update View", icon("refresh"))

      ),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Extreme Values",fluidRow(column(12,br()), column(12,plotlyOutput('extremePlot', height = 500))
                                                       , column(12,br()),column(4,DT::dataTableOutput('extremeTable')))),
                    tabPanel("Validation Plots", plotOutput("valPlot")),
                    tabPanel("Return Period Curve", downloadButton('downloadRP'), plotOutput("returnPlot"))
        )
      )
    )
  )






  # Define server logic for random distribution app ----
  server <- function(input, output) {

    # Load supporting code -- hardcoded SVN version
    source("\\\\WHKFAP02\\Projects\\Applications\\Data Services\\Data Services\\Environmental Data Services\\Tools\\DataServices\\R\\Packages\\timeseries_client.R")


    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- reactive({

      # Connect to the AQTS server
      timeseries$connect(config$server, config$username, config$password)


      # Get the location data
      locationData = timeseries$getLocationData(timeseries$getLocationIdentifier(input$timeSeriesName))
      utcOffset = timeseries$getUtcOffsetText(locationData$UtcOffset)

      startOfDay = "T00:00:00"
      endOfDay = "T23:59:59.9999999"

      # event period
      fromperiodStart = paste0(input$daterange[1], startOfDay, utcOffset)
      toperiodEnd = paste0(input$daterange[2], endOfDay, utcOffset)



      showNotification(id = "wait", "Reading and processing data from AQUARIUS. Please Wait...", duration = NULL, type = "message")
      #read time-series from AQ API
      json <- timeseries$getTimeSeriesCorrectedData(c(input$timeSeriesName),
                                           queryFrom = fromperiodStart,
                                           queryTo = toperiodEnd)

      periodLabel = sprintf("%s - %s", input$daterange[1], input$daterange[2])
      unit = json$Unit[1]
      titlem = paste("Annual peaks for", locationData$LocationName, "Site during:\n", periodLabel)



      if (json$NumPoints > 0) {
        # convert AQTS timestamps to POSIXct values
        timeStamp2 = strptime(substr(json$Points$Timestamp,0,19), "%FT%T")
        TS = zoo(json$Points$Value, timeStamp2)

        #calculate annual max from original frequency and then annual
        # I don't know why subdaily2annual doesn't work
        TSD1 = subdaily2daily(TS, FUN=max, na.rm = TRUE)
        TSD2 = daily2annual(TSD1, FUN=max, na.rm = TRUE)
        tm = format(as.Date(index(TSD2), format="%d/%m/%Y"),"%Y")
        dat = data.frame(tm, coredata(TSD2), unit, titlem )
        names(dat) = c("x","y","unit","titlem")


        dat
        removeNotification(id = "wait")


      } else {
        # No data
        dat = NA
      }

      dat

    })

    fitm <- reactive({
      fito=fevd(y, data = d(), type = input$dist , method = input$method ,threshold = 0.001)
    })


    # Generate a plot of the data ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output$extremePlot <- renderPlotly({
      dist <- input$dist
      n <- 20
      #s = input$x1_rows_selected
      p = plot_ly(d(),x=~x,y=~y, type = 'scatter', mode = 'lines')
      p = layout(p, title = ~titlem , xaxis=list(title=""), yaxis=list(title=~unit), showlegend = FALSE)
      add_trace(p, d(),x=~x,y=~y, type = 'scatter', mode = 'markers')

    })

    # Generate validation plots ----
    output$valPlot <- renderPlot({
      #data(Fort)
      #bmFort <- blockmaxxer(Fort, blocks = Fort$year, which="Prec")
      #fitGEV <- fevd(y, data = d())
      par(mfrow=c(2,2))
      plot(fitm(), type="prob", main = NA)
      plot(fitm(), type="qq", main = NA)
      #plot(fitm(), type="qq2", main = NA)
      plot(fitm(), type="density", main = NA)
      plot(fitm(), type="rl", main = NA)
      title(main="\n\nDensity Estimation Validation Curves",outer=T)
    }, height = 800)

    # Generate an HTML table view of the data ----
    output$extremeTable <- DT::renderDataTable({
      dtf = data.frame(d()$x, d()$y)
      names(dtf) = c("time","values")

      DT::datatable(dtf)
    })

    # Generate validation plots ----
    output$returnPlot <- renderPlot({
      #fitGEV <- fevd(y, data = d())
      plot(fitm(), type="rl" , main="Return Period Curve")
      grid()
    }, height = 700)

    output$downloadRP = downloadHandler(
      filename = "returncurve.pdf",
      content = function(file) {
        pdf(file = file)
        plot(fitm(), type="rl", main="Return Period Curve")
        grid()
        dev.off()
      })
  }


  # Create Shiny app ----
  shinyApp(ui, server)
}
