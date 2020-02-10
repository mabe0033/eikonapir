# changed by Martin Becker

#' get_symbology
#'
#' Returns a list of instrument names converted into another instrument code.
#'
#' For example: convert SEDOL instrument names to RIC names
#' @param symbol character or list of characters.
#'   Single instrument or list of instruments to convert.
#' @param from_symbol_type character.
#'   Instrument code to convert from.
#'   Possible values: 'CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker' (Default 'RIC')
#' @param to_symbol_type character or list.
#'   Instrument code to convert to.
#'   Possible values: 'CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker'
#' @param raw_output boolean.
#'   Set this parameter to \code{TRUE} to get the data in json format,
#'   if set to \code{FALSE}, the function will return a \code{data.frame}.
#'   The default value is \code{FALSE}.
#' @param debug boolean.
#'   If set to \code{TRUE}, the json request and response are printed.
#' @return \code{data.frame} containing the converted symbols.
#'
#' @examples
#' \dontrun{
#' get_symbology(list("MSFT.O", "GOOG.O", "IBM.N"), from_symbol_type="RIC", 
#'   to_symbol_type="ISIN")
#'}
#' @export
get_symbology <- function(symbol, from_symbol_type='RIC', to_symbol_type=NULL, 
                          raw_output=FALSE, debug=FALSE) {

  if (is.character(to_symbol_type)) to_symbol_type <- list(to_symbol_type)
  if (is.character(symbol)) symbol <- list(symbol)
  if (!is.character(from_symbol_type)) {
    warning("from_symbol_type must be a character string")
    return(NULL)
  }

  payload <- list('symbols'=symbol,'from'=from_symbol_type,'to'=to_symbol_type,
                  'bestMatchOnly'=TRUE)
  json_data = send_json_request("SymbologySearch",payload,debug)
  
  if (raw_output) json_data else {
    data = jsonlite::fromJSON(json_data)
    data.frame("Symbol"=data$mappedSymbols$symbol,data$mappedSymbols$bestMatch)
  }
}
