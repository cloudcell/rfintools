## Description: functions for extracting data from Bloomberg's website
##              http://www.bloomberg.com/quote/
##              including Bloomberg Industry Classification Standard
##              classification based on a Bloomberg symbol provided
## Author: cloudcello
## Date:   20151018
##
## TODO: add error handling
##

# Some Useful Data Fields:
#  securityType
#  name
#  primaryExchange
#  issuedCurrency
#  priceMinDecimals
#  nyTradeStartTime
#  nyTradeEndTime
#  timeZoneOffset
#  BICS fields, e.g.:
#  bicsSector":"Energy","bicsIndustry":"Oil, Gas & Coal","bicsSubIndustry":"Integrated Oils",
#  id
#  marketStatus
#  ultimateParentTicker


# get free Bloomberg data based on a Bloomberg symbol
# (BICS (bloomberg industry classification system) data and more)
#' @export
getInfo.Bloomberg <- function( symbol=NULL ) {


  # ############################################
  # for testing
  # symbol='MSFT:US'
  # 
  # ############################################

  require(XML)
  require(rjson) # TODO: compare to RJSONIO (is it more common to use RJSONIO?)
  CHARSET_BLMBRG="UTF8"

  if(is.null(symbol)) stop ("No symbol provided!")

  url = paste0("http://www.bloomberg.com/quote/", symbol)

  # url = "http://www.bloomberg.com/quote/{ticker}"
  doc <- htmlParse(url)

  # get all scripts from the page
  # nodes separator </div> == //div, belonging to type/class {specified}
  # in this case, </script> , belonging to "type"="text/javascript"
  doc_snip <- lapply(doc['//script[@type="text/javascript"]'],FUN=xmlValue) # lapply(X, FUN, ...)
  # commented out: let the next step handle the 'search'
  # doc_snip <- doc_snip[[26]]
if(0) {
  # sometimes a part of the list is neither text nor NULL, so iconv complains (inside regmatches)
  # doc_snip <- doc_snip[[7]] # TODO rewrite this hack
  # doc_snip[[10]]

  doc_snip<-doc_snip[lapply(doc_snip,nchar)>0]

  doc_snip<-lapply(doc_snip,charToRaw)
  doc_snip<-iconv(doc_snip, from=CHARSET_BLMBRG, to="ASCII")
}    
  # str(doc_snip)
  json_data <- regmatches(doc_snip, regexpr('bootstrappedData.*', doc_snip))
  # json_data <- regmatches(doc_snip, regexpr('clientApp\.start.*', doc_snip))
  # str(json_data)

  nch1<-nchar("bootstrappedData: ")+1
  # nch1<-nchar("clientApp.start(")+1
  nch2<-nchar(json_data)#-nchar(");")
  json_data_adj <- substr(json_data,nch1,nch2)
  # json_data_adj <- paste0("{",json_data,"}")

  # str(fromJSON(json_data_adj))

  # the input data has extra ('unpaired') braces, parenthesis,
  # etc. at the very end, but fromJSON seems to throw them away
  output_raw <- fromJSON(json_data_adj)
  # names(output_raw)<-"d" #data
  # output_raw[[1]]
  zz <- output_raw[[2]]
  # setwd("c:/tmp")
  # save(output_raw, file='zz', ascii=TRUE)
  zz
}

# gets BICS data based on a Bloomberg symbol
getBICS <- function( symbol=NULL ) {
  if(is.null(symbol)) stop ("No symbol provided!")

  data <- getInfo.Bloomberg(symbol)

  output <- vector(mode="character", length=3)
  # output <- as.list(mode="character", length=7)
  names(output) <- c("bicsSector", "bicsIndustry", "bicsSubIndustry")

  output["bicsSector"]      <- data$detailedQuote$bicsSector
  output["bicsIndustry"]    <- data$detailedQuote$bicsIndustry
  output["bicsSubIndustry"] <- data$detailedQuote$bicsSubIndustry

  output
}

# for testing (to be removed)
if(0){
  getBICS("MSFT:US")

  symbol_data <- getInfo.Bloomberg("MSFT:SW")
  str(symbol_data)

  symbol_data$securityType
}
