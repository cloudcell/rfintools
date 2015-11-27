
# Description: creates a graph representation of a trading strategy
#              created within quantstrat framework
# Date: 2015-11-20
# Author: cloudcello
#
# Version: pre-alpha stage
#


require(quantstrat)
# require(diagram)
require(igraph)


demo(luxor.1.strategy.basic)
demo(rsi)
demo(faber)

strategy.st <- "luxor"
strategy.st <- "RSI"
strategy.st <- stratRSI
strategy.st <- "faber"

getStrategy("RSI")

strategy.st <- getStrategy(strategy.st)
strategy.st <- getStrategy("stratRSI", envir = parent.frame())
strategy.st <- stratRSI

if(0){
# the following is just a demonstration
# 'su' == strategy 'unit'
st <- strategy.st
st <- stratRSI
  su <- st$indicators
  for (i in 1:length(su)) {
    print(su[[i]]$label)
  }
  str(su)
}


getNodesIndicators <- function(strategy, verbose=FALSE) {
  # lbl_out <- "~" #"oSi"
  # lbl_inp <- "~" #"iSi"

  # hack for the 'rsi' qs demo:
   if(inherits(strategy,"strategy")){
     st <- strategy
   } else {
     #assume it's a strategy 'handle'
    st <- getStrategy(strategy)
   }

    su <- st$indicators
    node_nbr <- length(su)
    result_final <- vector(mode="list", length = node_nbr)
    for (i in seq(node_nbr)) {

        outp <- paste(su[[i]]$label, sep = '~')
        if(verbose) print(outp)
        outp_len <- 1L

        su2 <- su[[i]]$arguments #$columns

        #inp -- inputs into the 'node'
        inp_len <- length(su2) # how many inputs there are
        linkdata_len <- length(outp_len) + length(inp_len)
        result <- vector(mode = "character", length = linkdata_len)
        result[1] <- outp
        for (ii in seq(inp_len)) {
            inp <- (paste(outp,as.expression(su2[[ii]]), sep = '~'))
            if(verbose) print(inp)
            result[1+ii] <- inp
        }
        if(verbose) print(result)
        result_final[[i]] <- result
    }
    result_final
}

getNodesSignals <- function(strategy, verbose=FALSE) {
  # lbl_out <- "~" #"oSi"
  # lbl_inp <- "~" #"iSi"
  # hack for the 'rsi' qs demo:
  if(inherits(strategy,"strategy")){
    st <- strategy
  } else {
    #assume it's a strategy 'handle'
    st <- getStrategy(strategy)
  }
  # strategy structure 'unit'
    su <- st$signals
    # str(su)
    node_nbr <- length(su)
    result_final <- vector(mode="list", length = node_nbr)
    for (i in seq(node_nbr)) {

        outp <- paste(su[[i]]$label, sep = '~')
        if(verbose) print(outp)
        outp_len <- 1L

        # su2 <- su[[i]]$arguments$columns
        su2 <- unlist(su[[i]]$arguments)
        su2 <- su2[names(su2)!="label"]

        inp_len <- length(su2)
        if(inp_len==0) stop("cannot find inputs in this signal")

        linkdata_len <- length(outp_len) + length(inp_len)
        result <- vector(mode = "character", length = linkdata_len)
        result[1] <- outp
        for (ii in seq(inp_len)) {
            inp <- (paste(su2[[ii]], sep='~'))
            if(verbose) print(inp)
            result[1+ii] <- inp
        }
        if(verbose) print(result)
        result_final[[i]] <- result
    }
    result_final
}

getNodesRules <- function(strategy, verbose=FALSE) {
  # lbl_out <- "~" # "oRu"
  # lbl_inp <- "~" # "iRu"
  # hack for the 'rsi' qs demo:
  if(inherits(strategy,"strategy")){
    st <- strategy
  } else {
    #assume it's a strategy 'handle'
    st <- getStrategy(strategy)
  }    # su <- st$rules$exit

    getNodeRuleType <- function(su=su, ...) {
        node_nbr <- length(su)
        if(node_nbr==0) return(NULL)
        result_final <- vector(mode="list", length = node_nbr)
        for (i in seq(node_nbr)) {

            outp <- paste(su[[i]]$type,su[[i]]$label, sep = '~')
            if(verbose) print(outp)

            outp_len <- 1L

            su2 <- su[[i]]$arguments$sigcol #$columns

            #inp -- inputs into the 'node'
            inp_len <- length(su2) # how many inputs there are
            linkdata_len <- length(outp_len) + length(inp_len)
            result <- vector(mode = "character", length = linkdata_len)
            result[1] <- outp
            for (ii in seq(inp_len)) {
                inp <- (paste(as.expression(su2[[ii]]), sep = '~'))
                if(verbose) print(inp)
                result[1+ii] <- inp
            }
            if(verbose) print(result)
            result_final[[i]] <- result
        }
        result_final
    }

    c(getNodeRuleType(st$rules$enter),getNodeRuleType(st$rules$exit),getNodeRuleType(st$rules$order))
}

indics <- getNodesIndicators(strategy.st)
sigs <- getNodesSignals(strategy.st, verbose = TRUE)
rules <- getNodesRules(strategy.st)


if(0) {
  add.rule()
  su <- st$indicators
  str(su)

  su <- st$rules
  str(su)


  indics
  str(indics)
  sigs
  rules

  str(st$rules$enter)
  str(rules)
}

parsable_data <- c(indics, sigs, rules)

# create parsable_data_tuples

getTuples <- function(parsable_data=parsable_data){
    result <- NULL
    for(i in seq(length(parsable_data))) {
        tuple_input <- parsable_data[[i]]
        tuples_nbr <- length(tuple_input)-1
        if(tuples_nbr<1L) { stop(paste0("strange input ", tuple_input)) }
        tuples <- vector(mode="list", length = tuples_nbr)
        for(ii in seq(tuples_nbr)) {
            tuple <- c(tuple_input[1],tuple_input[ii+1])
            tuples[[ii]] <- tuple
        }
        result <- c(result,tuples)
    }
    result
}


tuples <- getTuples(parsable_data)
str(tuples)


pdunlisted <- unlist(parsable_data)

# populate matrix by unique values
pduniq <- unique(pdunlisted)
node_nbr <- length(pduniq)
M  <- matrix(nrow = node_nbr, ncol = node_nbr, byrow = TRUE, data = 0)
colnames(M) <- pduniq
rownames(M) <- pduniq
M



# connect nodes based on parsable_data_tuples

# find the address within the matrix based on tuple & establish a link
# M["lbl.nSlow","lbl.30"]

tuples_nbr <- length(tuples)

for(i in seq(tuples_nbr)) {
    from_name <- tuples[[i]][1]
    # cl_name <- tuples[[8]][1]
    to_name <- tuples[[i]][2]
    M[ to_name, from_name ] <- "fw"
}



M

# plotmat(M, pos = c(4,2,2,2,2))
# plotmat(M)#, pos = c(4,2,2,2,2))
#
# plotmat(M, box.type="none",pos = c(4,2,2,4))
# plotmat(M,
#         box.type="rectangle", box.size = 0.1,
#         pos = c(2,1,2,1,2,4), arr.type="triangle")


# Every time you are creating plots you might get this error - "Error in
# plot.new() : figure margins too large". To avoid such errors you can first
# check par("mar") output. You should be getting:
par("mar")
# [1] 5.1 4.1 4.1 2.1
# To change that write:
par(mar=c(1,1,1,1))

net=graph.adjacency(M,mode="directed",weighted=TRUE,diag=TRUE)

plot.igraph(net,vertex.label=V(net)$name,
            layout=layout.fruchterman.reingold,
            vertex.label.color="black",
            edge.color="black",
            edge.width=E(net)$weight/3,
            edge.arrow.size=0.5
            )



