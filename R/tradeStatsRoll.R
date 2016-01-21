# rework as slidingTradeStats / tradeStatsRoll(ing)
# keep 'anchored' argument as well !

# TODO: modify audit so it could be used to backup calc's
# & propose this as an extension

#----------------------------------------------------------------------------- -
# slidingTradeStats: function prototype
# steps:
# 1a0. Load prices
# 1b0. Load portfolio (see PosPL) data, and base calculations on time index
#     in there, not the time index in prices.
# 2a0. Use 'endpoints' to determine intervals to plug into 'Dates' in the
#     scoped tradeStats (tradeStatsExt)
#     TODO: create a patch for 'scoped' tradeStats() ----
#     ( use endpoints() the same way as updatePortf() does )
# 3a0. Create a vector of lists with the number equal to the number of
#     intervals calculated in the previous step(s).
#     - use the following structure:
#         * scope.timeFirst
#         * scope.timeLast
#         * tradeStats
# 4a0. Process the 'portfolio' to produce slidingTradeStats
#
# Draft #2 ------ -
# 0a. setting inputs
# 1a. load prices
# 2a. load portfolio
# 3a. use PosPL and endpoints() to generate the overall index of the data
# 4a. calculate and store index (or even retrieved time, via index()) locations
#     to be used with tradeStats
# 5a. use tradeStats
#----------------------------------------------------------------------------- -

# based on 'Rolling Walk Forward Analysis' qunatstrat::walk.forward()

tradeStatsRoll <- function(Portfolios, # allow for plural
                           Symbols,
                           period,
                           anchored=FALSE,
                           k.roll,
                           ...) # additional arguments for tradeStats/tradeStatsExt function
{
    if(0) {
        # step 0a.
        # setting inputs
        period = "days"
        k.roll = 3 # rename to k.roll
        anchored = FALSE

        # step 2a.
        # load testdata & develop using that data
        endpoints(portf2$symbols$GBPUSD$posPL, on="days")

        # portfolio <- .getPortfolio(portfolio.st)
        # results <- list()

        portfolio <- portf2
    }

    # tmp hack:

    # TODO: use this for QS reference ----
    # Work with portfolios require 'companion' objects, such as market data,
    # instruments; therefore it makes no sense to make functions be able to work
    # with object portfolios directly. Most of the functions require portfolio
    # 'handles' and many of the functions ONLY support 'handles' of portfolios
    # instead of portfolio objects themselves.

    pname <- Portfolios
    Portfolio<-.getPortfolio(pname)

    portfolio <- Portfolio

    # assuming that timespans for all portfolio symbols are same, so ok to use 1st symbol to calculate end points
    # browser()
    symbol.st <- first(ls(portfolio$symbols))
    ppl       <- portfolio$symbols[[symbol.st]]$posPL

    # TODO: remove the 'init' record

    # ppl <- get(symbol.st)$PosPL
    if(0) head(symbol)

    # replace symbol with PosPL throughout

    # ep <- endpoints(symbol, on=period)
    ep <- endpoints(ppl, on=period)
    cat(ep,"\n")

    head(index(ppl))
    tail(index(ppl))

    # # total ??? why did they choose this puzzling name? # maybe 'testing' start TODO: propose renaming ----
    # total.start <- ep[1 + k.training] + 1
    # total.timespan <- paste(index(ppl[total.start]), '', sep='/') # beginning of timespan for testing

    if(anchored)
        trStData.start <- ep[1] + 1 # "training" shall mean 'inputData.start'

    results <- list()

    k <- 1
    while(TRUE)
    {
        result <- list()

        # start and end of rolling stats window
        if(!anchored)
            trStData.start <- ep[k] + 1
        trStData.end   <- ep[k + k.roll]

        # stop if training.end is beyond last data
        if(is.na(trStData.end))
            break

        trStData.timespan <- paste(index(ppl[trStData.start]),
                                   index(ppl[trStData.end]),
                                   sep='/')

        # TODO: skip, if the ending

        cat("timespan: ", trStData.timespan, "\n")

        result$trStData.timespan <- trStData.timespan

        print(paste('=== generating stats on', trStData.timespan))

        # run tradeStats on stats window
        # result$trStResult
        tradeStats.list   <- tradeStatsExt( Portfolios  =Portfolios,
                                            Symbols     =Symbols,
                                            Dates       =trStData.timespan,
                                            use         =c('txns','trades'),
                                            tradeDef    ='flat.to.flat',
                                            inclZeroDays=FALSE,
                                            debugF      =TRUE)

        if(0) print(tradeStats.list)

        # tradeStats.list <- result$apply.paramset$tradeStats

        if(is.null(tradeStats.list))
            warning(paste('no trades in rolling window',
                          trStData.timespan,
                          '; skipping test'))

        k <- k + 1

        result <- tradeStats.list

        results[[k]]<- result
        # results[[k]] <- tradeStats.list

        # k <- k + k.roll
    }
    results
}

if(0) {
    # testing
    put.portfolio("forex",portf2)
    # tradeStatsRoll(Portfolios =  portf2,Symbols = c('GBPUSD'),period = 'days', k.roll = 3)
    tradeStatsRoll(Portfolios =  "forex",Symbols = c('GBPUSD'),period = 'days', k.roll = 3)

    out <- tradeStatsRoll(Portfolios =  c("forex"), Symbols = c('GBPUSD'),period = 'days', k.roll = 30)
    str(out)
    print(out)
}

