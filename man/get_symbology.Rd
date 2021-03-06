% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/symbology.R
\name{get_symbology}
\alias{get_symbology}
\title{get_symbology}
\usage{
get_symbology(
  symbol,
  from_symbol_type = "RIC",
  to_symbol_type = NULL,
  raw_output = FALSE,
  debug = FALSE
)
}
\arguments{
\item{symbol}{character or list of characters.
Single instrument or list of instruments to convert.}

\item{from_symbol_type}{character.
Instrument code to convert from.
Possible values: 'CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker' (Default 'RIC')}

\item{to_symbol_type}{character or list.
Instrument code to convert to.
Possible values: 'CUSIP', 'ISIN', 'SEDOL', 'RIC', 'ticker'}

\item{raw_output}{boolean.
Set this parameter to \code{TRUE} to get the data in json format,
if set to \code{FALSE}, the function will return a \code{data.frame}.
The default value is \code{FALSE}.}

\item{debug}{boolean.
If set to \code{TRUE}, the json request and response are printed.}
}
\value{
\code{data.frame} containing the converted symbols.
}
\description{
Returns a list of instrument names converted into another instrument code.
}
\details{
For example: convert SEDOL instrument names to RIC names
}
\examples{
\dontrun{
get_symbology(list("MSFT.O", "GOOG.O", "IBM.N"), from_symbol_type="RIC", 
  to_symbol_type="ISIN")
}
}
