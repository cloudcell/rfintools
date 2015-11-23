
# Description: creates a graph representation of a trading strategy
#              created within quantstrat framework
# Date: 2015-11-20
# Author: cloudcello
#

require(quantstrat)
# require(diagram)
require(igraph)

par(mar=c(1,1,1,1))

demo(luxor.1.strategy.basic)
# demo(faber)

strategy.st <- "luxor"
# strategy.st <- "faber"

# lapply(getStrategy(strategy.st)$indicators,names)
# lapply(getStrategy(strategy.st),names)


st <- getStrategy(strategy.st)

# 'su' == strategy 'unit'
# the following is just a demonstration
if(0){
  su <- st$indicators
  for (i in 1:length(su)) {
    print(su[[i]]$label)
  }
  str(su)
}



su <- st$indicators
str(su)

su <- st$rules
str(su)


getNodesIndicators <- function(strategy, verbose=FALSE) {
  # lbl_out <- "~" #"oSi"
  # lbl_inp <- "~" #"iSi"
    st <- getStrategy(strategy)
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

indics <- getNodesIndicators(strategy.st)
indics
str(indics)

getNodesSignals <- function(strategy, verbose=FALSE) {
  # lbl_out <- "~" #"oSi"
  # lbl_inp <- "~" #"iSi"
    st <- getStrategy(strategy)
    su <- st$signals
    node_nbr <- length(su)
    result_final <- vector(mode="list", length = node_nbr)
    for (i in seq(node_nbr)) {

        outp <- paste(su[[i]]$label, sep = '~')
        if(verbose) print(outp)
        outp_len <- 1L

        su2 <- su[[i]]$arguments$columns

        inp_len <- length(su2)
        linkdata_len <- length(outp_len) + length(inp_len)
        result <- vector(mode = "character", length = linkdata_len)
        result[1] <- outp
        for (ii in seq(inp_len)) {
            inp <- (paste(su2[[ii]], sep = '~'))
            if(verbose) print(inp)
            result[1+ii] <- inp
        }
        if(verbose) print(result)
        result_final[[i]] <- result
    }
    result_final
}

sigs <- getNodesSignals(strategy.st)
sigs

getNodesRules <- function(strategy, verbose=FALSE) {
  # lbl_out <- "~" # "oRu"
  # lbl_inp <- "~" # "iRu"
    st <- getStrategy(strategy)
    # su <- st$rules$exit

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

rules <- getNodesRules(strategy.st)
rules

str(st$rules$enter)
str(rules)

parsable_data <- c(indics, sigs, rules)

pdunlisted <- unlist(parsable_data)

# populate matrix by unique values
pduniq <- unique(pdunlisted)
node_nbr <- length(pduniq)
M  <- matrix(nrow = node_nbr, ncol = node_nbr, byrow = TRUE, data = 0)
colnames(M) <- pduniq
rownames(M) <- pduniq
M



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

# plotweb(M)
