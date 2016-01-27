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

# arguments 'portfolios', 'symbols' are passed further to the tradeStats()
# function; however
# this function uses the assumption that all portfolios are have been tested
# on the same time period
tradeStatsRoll <- function(Portfolios, # allow for plural
                           Symbols,
                           period,
                           anchored=FALSE,
                           k.span, # rename to k.span
                           k.step=1, # number of periods to 'walk forward'
                           ...) # additional arguments for tradeStats/tradeStatsExt function
{
    if(0) {
        # step 0a.
        # setting inputs
        period = "days"
        k.span = 3 # rename to k.span
        anchored = FALSE

        # step 2a.
        # load testdata & develop using that data
        endpoints(portf2$symbols$GBPUSD$posPL, on="days")

        # portfolio <- .getPortfolio(portfolio.st)
        # results <- list()

        portfolio <- portf2
    }

    if(inherits(Portfolios, "portfolio") || !inherits(Portfolios[1], "character")) {
        stop(paste("Use put.portfolio() to place the portfolio into the",
                   "appropriate environment first.",
                   "Use portfolio 'handles' only."))

    }

    overallResult <- list()

    portfNum <- 1 # max == length(Portfolios)

    for (Portfolio in Portfolios){
        # TODO: use this for QS reference ----
        # Work with portfolios require 'companion' objects, such as market data,
        # instruments; therefore it makes no sense to make functions be able to work
        # with object portfolios directly. Most of the functions require portfolio
        # 'handles' and many of the functions ONLY support 'handles' of portfolios
        # instead of portfolio objects themselves.

        # TODO: think whether I need to break down by portfolio at THIS stage ----
        # instead of passing this step into tradeStats() later
        # Pros:
        #   This list may include a vector of any portfolios - so makes more universal
        #
        pname <- Portfolio
        Portfolio<-.getPortfolio(pname)

        portfolio <- Portfolio # TODO refactor later

        # assuming that timespans for all portfolio symbols are same,
        # so ok to use 1st symbol to calculate end points
        # browser()
        symbol.st <- first(ls(portfolio$symbols))
        ppl       <- portfolio$symbols[[symbol.st]]$posPL

        # TODO: remove the 'init' record ----

        ep <- endpoints(ppl, on=period)
        cat(ep,"\n")

        cat('input data head records (timestamps only) :\n')
        print( head(index(ppl)) )
        cat('input data tail records (timestamps only) :\n')
        print( tail(index(ppl)) )

        if(anchored)
            trStData.start <- ep[1] + 1 # "training" shall mean 'inputData.start'

        portfResults <- list() # overall result (function output)

        k <- 1 # span start
        while(TRUE)
        {
            tsResult <- list() # the current "time span result" (portfolio:symbol:timespan)

            # start and end of rolling stats window
            if(!anchored)
                trStData.start <- ep[k] + k.step
            trStData.end       <- ep[k + k.span]

            # stop if training.end is beyond last data
            if(is.na(trStData.end))
                break

            trStData.timespan <- paste(index(ppl[trStData.start]),
                                       index(ppl[trStData.end]),
                                       sep='/')

            # TODO: skip, if end of data has been reached

            cat("timespan: ", trStData.timespan, "\n")

            tsResult$trStData.timespan <- trStData.timespan

            print(paste('=== generating stats on', trStData.timespan))

            # inside tradeStats:
            #     for each portfolio
            #       for each symbol
            #          {generate trade stats}
            # (see 'control structure prototype' at the bottom of the file)
            tradeStats.list   <- tradeStatsExt( Portfolios   = Portfolios,
                                                Symbols      = Symbols,
                                                Dates        = trStData.timespan,
                                                use          = c('txns','trades'),
                                                tradeDef     = 'flat.to.flat',
                                                inclZeroDays = FALSE,
                                                debugF       = TRUE)

            if(is.null(tradeStats.list))
                warning(paste('no trades in rolling window', trStData.timespan,
                              '; skipping test'))

            tsResult <- tradeStats.list

            portfResults[[k]]<- tsResult

            k <- k + k.step
        }

        if(k.step!=1) {
            lapply(portfResults, FUN=function(x){ if(is.na(x)){ x<-NULL } })
        }

        overallResult[[portfNum]] <-  portfResults
        portfNum <- portfNum + 1
    }

    overallResult
}

if(0) {
    # testing
    put.portfolio("forex",portf2)

    # import portf2
    out <- tradeStatsRoll(Portfolios =  c("forex"), Symbols = c('GBPUSD'),period = 'days', k.span = 5)
    str(out)
    print(out)


    ## CONTROL STRUCTURE PROTOTYPE:
    {
        overallResult <- list()

        portfNum <- 1 # max == length(Portfolios)

        ## for each portfolio
        for (Portfolio in Portfolios){

            portfResults <- list() # overall result (function output)

            k <- 1 # span start
            ## for a selected symbol
            while(TRUE)
            {
                tsResult <- list() # the current "time span result"
                # [ portfolio : timespan : symbols ]

                k <- k + k.step

                tsResult <- tradeStats.list

                portfResults[[k]]<- tsResult
            }

            overallResult[[portfNum]] <-  portfResults
            portfNum <- portfNum + 1
        }

        overallResult
    }


}





