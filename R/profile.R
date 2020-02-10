# changed by Martin Becker

requestInfo <- new.env()
requestInfo$application_key <- ""
requestInfo$proxy_port      <- 9000L

#' set_app_key
#'
#' Use this function to set your application id.
#' The application id should be set before calling functions to retrieve data
#' @param appKey character string.
#'
#' @examples
#' set_app_key('YOUR_APP_KEY')
#' @export
set_app_key <- function(appKey) requestInfo$application_key <- appKey


#' get_app_key
#'
#' Use this function to get back the application id you have set earlier with 
#' set_app_key.
#'
#' @return single character string.
#' @examples
#' my_app_key <- get_app_key()
#' @export
get_app_key <- function() requestInfo$application_key


#' set_proxy_port
#'
#' By default the library will try to connect to the proxy default port 9000.
#' Use this function if the proxy is listening on another port than 9000
#' @param port numeric scalar (will be converted to integer).
#'
#' @examples
#' set_proxy_port(37009)
#' @export
set_proxy_port <- function(port) requestInfo$proxy_port <- as.integer(port)


#' get_proxy_port
#'
#' Use this function to get back the proxy port the library will connect to
#'
#' @return scalar integer.
#' @examples
#' proxy_port = get_proxy_port()
#' @export
get_proxy_port <- function() requestInfo$proxy_port
