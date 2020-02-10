# changed by Martin Becker

#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr add_headers POST content
send_json_request <- function(entity, request_data, debug=FALSE) {
  if (typeof(request_data) == 'character')
    request_data = jsonlite::fromJSON(request_data)

  url = paste('http://localhost:',get_proxy_port(),'/api/v1/data',sep='')
  request <- list('Entity'= list('E'= entity, 'W'= request_data))
  response <- httr::POST(url, 
    httr::add_headers('Content-Type'='application/json',
                      'x-tr-applicationid'=get_app_key()),
    body=request, encode = "json")
  response_data <- httr::content(response, "text")
  response_status <- response$status_code

  if (debug) {
    print("Request *************************************")
    print(jsonlite::toJSON(request))
    print("Response *************************************")
    print(response_data)
    print("Response status *************************************")
    print(response_status)
  }

  if (response$status_code == 200) response_data else {
     warning("HTTP Error, code= ", response$status_code, sep="")
     NULL
  }
}
