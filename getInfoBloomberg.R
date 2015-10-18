# get free Bloomberg data based on a ticker 
# (BICS (bloomberg industry classification system) data and more)
getInfo.Bloomberg <- function( ticker=NULL ) {
  require(XML)
  require(rjson) # TODO: compare to RJSONIO (is it more common to use RJSONIO?)

  if(is.null(ticker)) stop ("No ticker provided!")

  url = paste0("http://www.bloomberg.com/quote/", ticker)

  # url = "http://www.bloomberg.com/quote/{ticker}"
  doc <- htmlParse(url)

  # get all scripts from the page
  # nodes separator </div> == //div, belonging to type/class {specified}
  # in this case, </script> , belonging to "type"="text/javascript"
  doc_snip <- lapply(doc['//script[@type="text/javascript"]'],FUN=xmlValue) # lapply(X, FUN, ...)

  doc_snip <- doc_snip[[26]]

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
  output_raw[[1]]
}
