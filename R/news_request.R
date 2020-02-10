# changed by Martin Becker

#' get_news_headlines
#'
#' Returns a data frame contianing the news headlines
#'
#' @param query string, optional. Search criteria for the news headlines
#'   The query can contain RIC codes, company names, country names and
#'   operators (AND, OR, NOT, IN, parentheses and quotes for explicit search…).
#'   Tip: Append 'R:' in front of RIC names to improve performance.
#'   Default: Top News written in English
#'
#' @param count integer, optional. The maximum number of headlines to retrieve.
#'   Value Range: [1-100].
#'   Default: 10
#' @param date_from character. Beginning of date range.
#'   String format is:'\%Y-\%m-\%dT\%H:\%M:\%S'. e.g. 2016-01-20T15:04:05.
#' @param date_to character. End of date range.
#'   String format is: '\%Y-\%m-\%dT\%H:\%M:\%S'. e.g. "2016-01-20T15:04:05".
#' @param raw_output boolean. Set this parameter to \code{TRUE} to get the data 
#'   in json format.
#'   If set to \code{FALSE}, the function will return a \code{data.frame}.
#'   The default value is \code{FALSE}.
#' @param debug boolean. If this parameter is set to True, the json request and response are printed.
#'
#' @examples
#' \dontrun{
#' headlines = get_news_headlines("R:MSFT.O", 2)
#' headlines = get_news_headlines("R:MSFT.O IN FRANCE")
#' headlines = get_news_headlines("R:MSFT.O IN FRANCE IN ENGLISH", count=5)
#' headlines = get_news_headlines("OBA* OR CLINTON IN ENGLISH", count=5)
#' }
#' @export get_news_headlines
get_news_headlines <- function (query='Topic:TOPALL and Language:LEN', count=10L, 
                                date_from=NULL, date_to=NULL, raw_output=FALSE, 
                                debug=FALSE)
{
  endpoint = "News_Headlines"

  if (!is.character(query)) {
    warning('get_news_headlines error: query must be a string')
    return (NULL)
  }
  
  count <- as.integer(count)
  if (!isTRUE((count>=0)&&(count<=100))) {
    warning('count must be between 0 and 100')
    return (NULL)
  }

  # build the payload
  payload = list('number'= toString(count), 'query'= query, 
                 'productName'=get_app_key(), 'attributionCode'= '')

  if (!is.null(date_from)) payload['dateFrom'] = date_from
  if (!is.null(date_to))   payload['dateTo'] = date_to

  json_data = send_json_request(endpoint, payload, debug)

  if (raw_output) json_data else {
    result <- jsonlite::fromJSON(json_data)
    headlines  <- result$headlines
    data_frame <- data.frame(headlines$firstCreated,headlines$versionCreated,
                             headlines$text,headlines$storyId,headlines$sourceCode)
    names(data_frame) <- c('firstCreated','versionCreated','text','storyId','sourceCode')
    data_frame
  }
}


#' get_news_story
#'
#' Return a single news story corresponding to the identifier provided in story_id
#'
#' @param story_id   The story id. The story id is a field you will find in every headline retrieved with the function get_news_headlines
#' @param raw_output  boolean. Set this parameter to True to get the data in json format
#' The default value is False
#' @param debug  boolean. When set to True, the json request and response are printed.
#'
#' @examples
#' \dontrun{
#' headlines <- get_news_headlines('IBM')
#' for (story_id in headlines$storyId) {
#'   story <- get_news_story(story_id)
#'   print (story)
#' }
#' }
#' @export get_news_story
get_news_story <- function(story_id,raw_output=FALSE, debug=FALSE) {

  payload <- list('attributionCode'='', 'productName'= get_app_key(), 
                  'storyId' =  story_id)
  json_data <- send_json_request("News_Story", payload, debug)

  if (raw_output) json_data else jsonlite::fromJSON(json_data)$story$storyHtml
}
