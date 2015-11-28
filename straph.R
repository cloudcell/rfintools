
# Description: creates a graph representation of a trading strategy
#              created within quantstrat framework
# Date: 2015-11-20
# Author: cloudcello
# Email: ef256aff@opayq.com
#
# Version: 0.0.1
#


require(quantstrat)
require(igraph)


# strategy.st <- "luxor"
# strategy <- getStrategy(strategy.st)

if(0){
  demo(luxor.1.strategy.basic)
  demo(rsi)
  demo(faber)
  demo(bee)
  demo(rocema)
  demo(pair_trade)
  demo(macd)


  strategy.st <- stratRSI
  strategy.st <- "faber"


  strategy <- getStrategy("stratRSI", envir = parent.frame())
  strategy <- getStrategy("bee", envir = parent.frame())
  strategy <- getStrategy("pairStrat", envir = parent.frame())
  strategy <- getStrategy("macd")
  strategy <- stratRSI

}



getNodesIndicators <- function(strategy, verbose=FALSE) {

    # hack for the 'rsi' qs demo:
    if(inherits(strategy,"strategy")){
        st <- strategy
    } else {
        #assume it's a strategy 'handle'
        st <- getStrategy(strategy)
    }
    # str(st)
    # 'su' == strategy 'unit'
    su <- st$indicators
    node_nbr <- length(su)
    result_final <- vector(mode="list", length = node_nbr)
    for (i in seq(node_nbr)) {

        outp <- paste(su[[i]]$label, sep = '~')
        if(verbose) print(outp)
        outp_len <- 1L

        # su2 <- su[[i]]$arguments #$columns
        su2 <- unlist(su[[i]]$arguments)
        su2 <- su2[names(su2)!="label"]
        su2 <- su2[su2!="TRUE"]
        su2 <- su2[su2!="FALSE"]

        #inp -- inputs into the 'node'
        inp_len <- length(su2) # how many inputs there are
        if(verbose) cat(paste0("# of args: ",inp_len,'\n'))
        if(inp_len==0) stop("cannot find inputs in this signal")

        linkdata_len <- (outp_len) + (inp_len)
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
        su2 <- su2[su2!="TRUE"]
        su2 <- su2[su2!="FALSE"]

        inp_len <- length(su2)
        if(verbose) cat(paste0("# of args: ",inp_len,'\n'))
        if(inp_len==0) stop("cannot find inputs in this signal")

        linkdata_len <- outp_len + inp_len
        result <- vector(mode = "character", length = linkdata_len)
        result[1] <- outp

        # result[seq(2:(inp_len+1))]<-su2[seq(inp_len)]

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
    # hack for the 'rsi' qs demo:
    if(inherits(strategy,"strategy")){
        st <- strategy
    } else {
        #assume it's a strategy 'handle'
        st <- getStrategy(strategy)
    }
    # su <- st$rules$exit

    getNodeRuleType <- function(su=su, ...) {
        node_nbr <- length(su)
        if(node_nbr==0) return(NULL)
        result_final <- vector(mode="list", length = node_nbr)
        for (i in seq(node_nbr)) {
            # i <- 1
            outp <- paste(su[[i]]$type,su[[i]]$label, sep = '~')
            if(verbose) print(outp)

            outp_len <- 1L

            # su2 <- su[[i]]$arguments$sigcol #$columns
            su2 <- unlist(su[[i]]$arguments)
            su2 <- su2[names(su2)!="label"]
            su2 <- su2[su2!="TRUE"]
            su2 <- su2[su2!="FALSE"]

            #inp -- inputs into the 'node'
            inp_len <- length(su2) # how many inputs there are
            if(verbose) cat(paste0("# of args: ",inp_len,'\n'))
            if(inp_len==0) stop("cannot find inputs in this signal")


            linkdata_len <- (outp_len) + (inp_len)
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

    c(getNodeRuleType(st$rules$enter),
      getNodeRuleType(st$rules$exit),
      getNodeRuleType(st$rules$order))
}

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




indics <- getNodesIndicators(strategy, verbose = TRUE)
sigs <- getNodesSignals(strategy, verbose = TRUE)
rules <- getNodesRules(strategy, verbose = TRUE)
parsable_data <- c(indics, sigs, rules)

# create parsable_data_tuples
tuples <- getTuples(parsable_data)
tuples_nbr <- length(tuples)

# populate matrix by unique values
pduniq <- unique( unlist(parsable_data) )
node_nbr <- length(pduniq)
M  <- matrix(nrow = node_nbr, ncol = node_nbr, byrow = TRUE, data = 0)
colnames(M) <- pduniq
rownames(M) <- pduniq
M


# establish links e.g.: M["lbl.nSlow","lbl.30"] <- 'link'
for(i in seq(tuples_nbr)) {
    from_name <- tuples[[i]][1]
    to_name <- tuples[[i]][2]
    M[ to_name, from_name ] <- "fw"
}
M

net=graph.adjacency(M,mode="directed",weighted=TRUE,diag=TRUE)

# set margins
par("mar")
par(mar=c(1,1,1,1))
par(ask=FALSE) # shut R up

plot.igraph(net,vertex.label=V(net)$name,
            layout=layout.fruchterman.reingold,
            vertex.label.color="black",
            edge.color="black",
            edge.width=E(net)$weight/3,
            edge.arrow.size=0.5
            )



