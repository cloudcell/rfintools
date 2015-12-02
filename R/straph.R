
# Description: creates a graph representation of a trading strategy
#              created within quantstrat framework
# Date: 2015-11-20
# Author: cloudcello
# Email: ef256aff@opayq.com
#
# Version: 0.0.1
#





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
            su2 <- su2[su2!="NULL"]
            su2names <- names(su2)

            #inp -- inputs into the 'node'
            inp_len <- length(su2names) # how many inputs there are

            if(verbose) cat(paste0("# of args: ", inp_len,'\n'))
            if(inp_len==0) stop("cannot find inputs in this signal")


            linkdata_len <- (outp_len) + (inp_len)
            result <- vector(mode = "character", length = linkdata_len)
            result[1] <- outp

            idx <- 2
            for (ii in su2names) {
                # inp <- (paste(as.expression(su2[[ii]]), sep = '~'))
                # inp <- paste0(as.expression(su2[[ii]]),"")
                inp <- su2[ii]
                # class(stratRSI$rules$enter[[1]]$arguments$osFUN)
                if(is.function(inp)) {inp <- ii} #assign arg name instead / FIXME
                if(verbose) print(inp)
                result[idx] <- inp
                idx <- idx + 1
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


plot.strategy <- function(strategy=NULL, envir = NULL) {
    require(quantstrat)
    require(igraph)

    if(!is.null(envir)){
        strategy <- get.strategy(strategy, envir = envir)
        # if(inherits(strategy,"strategy")){
        #     strategy <- strategy
        # } else {
        #     #assume it's a strategy 'handle'
        #     strategy <- getStrategy(strategy)
        # }
    }

    indics <- getNodesIndicators(strategy) #, verbose = TRUE)
    sigs <- getNodesSignals(strategy) #, verbose = TRUE)
    rules <- getNodesRules(strategy)#, verbose = TRUE)
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
    # M


    # establish links e.g.: M["lbl.nSlow","lbl.30"] <- 'link'
    for(i in seq(tuples_nbr)) {
        from_name <- tuples[[i]][1]
        to_name <- tuples[[i]][2]
        M[ to_name, from_name ] <- 1 # diagram package uses 'strings' like 'fw'
    }
    # M

    net <- graph.adjacency(M, mode="directed", weighted=NULL, diag=TRUE)

    opar <- par()
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

    # restore graphics parameters
    #
    owarn <- options("warn")
    options(warn=-1)
    # suppressWarnings(par(opar))
    par(opar)
    options(owarn)

}

plot.strategy.exp <- function(strategy=NULL, envir = NULL) {
    require(quantstrat)
    require(igraph)

    if(!is.null(envir)){
        strategy <- get.strategy(strategy, envir = envir)
        # if(inherits(strategy,"strategy")){
        #     strategy <- strategy
        # } else {
        #     #assume it's a strategy 'handle'
        #     strategy <- getStrategy(strategy)
        # }
    }

    indics <- getNodesIndicators(strategy) #, verbose = TRUE)
    sigs <- getNodesSignals(strategy) #, verbose = TRUE)
    rules <- getNodesRules(strategy)#, verbose = TRUE)
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
    # M


    # establish links e.g.: M["lbl.nSlow","lbl.30"] <- 'link'
    for(i in seq(tuples_nbr)) {
        from_name <- tuples[[i]][1]
        to_name <- tuples[[i]][2]
        M[ to_name, from_name ] <- 1 # diagram package uses 'strings' like 'fw'
    }
    # M

    net <- graph.adjacency(M, mode="directed", weighted=NULL, diag=TRUE)

    opar <- par()
    # set margins
    par("mar")
    par(mar=c(1,1,1,1))
    par(ask=FALSE) # shut R up

    # plot.igraph(net,vertex.label=V(net)$name,
    #             layout=layout.fruchterman.reingold,
    #             vertex.label.color="black",
    #             edge.color="black",
    #             edge.width=E(net)$weight/3,
    #             edge.arrow.size=0.5
    # )

    # taken from http://kateto.net/network-visualization
    # start ---
    tkid <- tkplot(net,
                   edge.arrow.size=.2,
                   edge.color="orange",
                   vertex.color="orange",
                   vertex.frame.color="#ffffff",
                   vertex.label=V(net)$media,
                   vertex.label.color="black") # tkid is the id of the tkplot that will open
    # l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
    # plot(net, layout=l)
    # end ---


    # restore graphics parameters
    #
    owarn <- options("warn")
    options(warn=-1)
    # suppressWarnings(par(opar))
    par(opar)
    options(owarn)

}



if(0){
    require(quantstrat)
    require(igraph)
    # plot.strategy(stratRSI)

    # plot.strategy("luxor")
    plot.strategy("pairStrat", envir = parent.frame())
    # plot.strategy("stratRSI", envir = parent.frame())
    strategy <- getStrategy("pairStrat", envir = parent.frame())



    # strategy.st <- "luxor"
    # strategy <- getStrategy(strategy.st)
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

    plot.strategy("luxor")
    plot.strategy.exp("luxor")



# install.packages("tkplot")
# library(tkplot)

    tkcreate(canvas, "text",
             width/2, 25,
             text="My title",
             justify="center",
             font=tcltk::tkfont.create(family="helvetica",size=20,weight="bold", color="red"))


tkid <- tkplot(net) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(net, layout=l)

}



