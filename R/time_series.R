# changed by Martin Becker

#' get_timeseries
#'
#' Returns historical data for one or several RICs
#'
#' @param rics  string or list of strings.
#' Single RIC or List of RICs to retrieve historical data for
#' @param fields  string or list of strings
#' Use this parameter to filter the returned fields set.
#  List of available fields: ['TIMESTAMP', 'VALUE', 'VOLUME', 'HIGH', 'LOW', 'OPEN', 'CLOSE', 'COUNT']
#  By default all fields are returned.
#
#' @param start_date string. The start date of the historical range
#' string format is: %Y-%m-%dT%H:%M:%S. ex: 2016-01-20T15:04:05.
#' Default: current date - 100 days
#' @param end_date string. The end date of the historical range.
#  string format is: %Y-%m-%dT%H:%M:%S. ex: 2016-01-20T15:04:05.
#  Default: current date
#' @param interval string. the data interval.
#' Possible values: 'tick', 'minute', 'hour', 'daily', 'weekly', 'monthly', 'quarterly', 'yearly' (Default 'daily')
#' @param count integer. The maximum number of data points you want tp retrieve.
#' @param calendar string. Possible values: 'native', 'tradingdays', 'calendardays'.
#' @param corax string. Possible values are : 'adjusted', 'unadjusted'
#' @param raw_output boolean
#' Set this parameter to TRUE to get the data in json format
#' if set to FALSE, the function will return a data frame which shape is defined by the parameter normalize
#' The default value is False
#' @param normalize boolean
#' if set to True, the function will return a normalized data frame with the following columns 'Date','Security','Field'
#' If the value of this parameter is False the returned data frame shape will have a different column for each field and a column
#' for the security
#' The default value is False
#' Remark: This parameter has a less precedence than the parameter rawOutput i.e. if rawOutput is set to True, the returned data will be the raw data and this parameter will be ignored
#' @param convert boolean. If set to \code{TRUE}, the columns of the \code{data.frame}
#' are converted to appropriate \code{R} data types (instead of \code{character}).
#' (Currently) defaults to \code{FALSE}.
#' @param debug boolean
#' When set to True, the json request and response are printed.
#'
#' @examples
#' \dontrun{
#' df <- get_timeseries(list("MSFT.O","VOD.L","IBM.N"),list("*"),
#'   "2016-01-01T15:04:05","2016-01-10T15:04:05","daily")
#' }
#' @export get_timeseries
get_timeseries <- function(rics,fields='*', start_date=NULL, end_date=NULL, 
                   interval='daily', normalize=FALSE, count=NULL,
                   calendar=NULL, corax=NULL,raw_output=FALSE, convert=FALSE,
                   debug=FALSE) {

  Calendar_Values <- c('native', 'tradingdays', 'calendardays')
  Corax_Values    <- c('adjusted', 'unadjusted')
  Interval_Values <- c('daily','hour','minute','weekly','monthly','quarterly',
                       'yearly','tick','taq')

  if (is.character(rics)) rics <- list(trimws(rics))

  fields <- if ( '*' %in% fields) list('*') else 
    if ('TIMESTAMP' %in% fields) fields else  c('TIMESTAMP',fields)

  if (is.null(start_date)) 
    start_date <- paste0(as.character(Sys.Date()-100),"T00:00:00") else
    start_date <- fixDate(start_date,start=TRUE)

  if (is.null(end_date)) 
    end_date <- paste0(as.character(Sys.Date()),"T23:59:59") else
    end_date <- fixDate(end_date,start=FALSE)

  payload <- list('rics'= rics, 'fields'= fields, 
                  'startdate' = start_date,'enddate'= end_date)

  count <- sanitize_integer(count,"count")
  if (!is.null(count)) payload$count <- count

  interval <- sanitize_string(interval,"interval")
  if (!is.null(interval)) {
		interval <- match.arg(interval,Interval_Values)  
		payload$interval <- interval
  }

  calendar <- sanitize_string(calendar,"calendar")
  if (!is.null(calendar)) {
    calendar <- match.arg(calendar,Calendar_Values)
    payload$calendar <- calendar
  }
  
  corax <- sanitize_string(corax,"corax")
  if (!is.null(corax)) {
    corax <- match.arg(corax,Corax_Values)
    payload$corax <- corax
  }
  
  json_data = send_json_request("TimeSeries", payload, debug=debug)

  if (raw_output) json_data else 
    getFormatted(jsonlite::fromJSON(json_data),convert,normalize)
}

#' @importFrom reshape2 melt
getFormatted <- function(dat,convert=FALSE,normalize=FALSE) {
  res <- NULL
  if (names(dat)=="timeseriesData") dat <- dat[[1]]
  for (i in seq_along(dat$dataPoints)) {
    if (dat$statusCode[i]!="Normal") 
      warning(paste0("Status code: ",dat$statusCode[i]," for RIC ",dat$ric[i]))
    tmp <- as.data.frame(dat$dataPoints[[i]],stringsAsFactors=FALSE)
    if (normalize) {
      colnames(tmp) <- dat$fields[[i]]$name
      tmp <- reshape2::melt(tmp,id.vars=c("TIMESTAMP"))
      tmp <- tmp[order(tmp[,1]),]
    }
    res <- rbind(res,cbind(tmp,RIC=dat$ric[i],stringsAsFactors=FALSE))
  }
  if (convert) {
    timestamp <- res[,1]
    timestamp <- gsub("Z","",timestamp)
    timestamp <- gsub("T"," ",timestamp)
    res[,1] <- as.POSIXct(timestamp)
    res[,ncol(res)] <- as.factor(res[,ncol(res)])
  }
  if (normalize) {
    res <- res[,c(1,4,2,3)]
    colnames(res) <- c("Date","Security","Field","Value")
    res[,4] <- as.numeric(res[,4])
    if (convert) for (i in 2:3) res[,i] <- as.factor(res[,i])
  } else {
    colnames(res) <- c(dat$fields[[1]]$name,"RIC")
    if (convert) for (i in seq(2,ncol(res)-1)) res[,i] <- as.numeric(res[,i])
  }
  rownames(res) <- seq_len(nrow(res))
  res
}

