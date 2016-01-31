# Original Code Source: QuantStrat (blotter) R package

# TODO: dig around online & find Guy Yollin's slides where
# he talked about observed differences in the backtest on
# TradeStation from what's in the book

# See sections marked with the following 'tag lines'
#---proposed extension-START-OF-SECTION--------------------------- -
#---proposed extension-END-OF-SECTION----------------------------- -

# Extensions proposed by 'cloudcell':
#  1. date/time filter
#  2. percent.time.in.market indicator solution

# FIXME: this function has only been tested on portfolios marked each bar
#        i.e. tick-level portfolios marked on a lower 'frequency' may misbehave
#' @export tradeStatsExt
tradeStatsExt <- function(Portfolios, Symbols, use=c('txns','trades'),
                          tradeDef='flat.to.flat',inclZeroDays=FALSE,
                          Dates=NULL,
                          ## Interval is necessary to process interval-updated
                          ## portfolio properly: to weed out position PL from
                          ## extra records (transaction table records merged
                          ## with interval-based position PL)
                          ## Type of input values: the same rules apply
                          ## as in .updatePosPL()
                          Interval,
                          debugF=FALSE)
{
    # if(!inherits(Portfolios,"portfolio")) {
    #     pname <- Portfolio
    #     Portfolio<-.getPortfolio(pname)
    # } else {
    #     pname <- "noNamePortfolio"
    # }

    ret <- NULL
    use <- use[1] #use the first(default) value only if user hasn't specified
    tradeDef <- tradeDef[1]
    for (Portfolio in Portfolios){
        # if(!inherits(Portfolios,"portfolio")) {
        pname <- Portfolio
        Portfolio<-.getPortfolio(pname)
        # } else {
        #     pname <- "noNamePortfolio"
        # }

        # browser()
        if(missing(Symbols)) symbols <- ls(Portfolio$symbols)
        else symbols <- Symbols

        ## Trade Statistics
        for (symbol in symbols){
            txn   <- Portfolio$symbols[[symbol]]$txn
            posPL <- Portfolio$symbols[[symbol]]$posPL
            #---proposed extension-START-OF-SECTION--------------------------- -
            # Removes the initial record, whether the stats are 'scoped' or not
            # ( not to remove info in the range '( t(-1); t(0) ]' )
            posPL <- posPL[-1,]

            # One might want to remove info in the range '( t(-1); t(0) ]'
            # as the first record may include 'information'
            # external to the scope, if OHLC/bar data is used.
            # E.g. a record timestamped "2002-02-21 00:00"
            #      for 30 minute-per-bar data will include events from
            #      "2002-02-20 23:30" until "2002-02-21 00:00"
            #      and, therefore, must be excluded.
            #
            # However, such a solution would not be correct for tick data.
            #
            # Assumption: all the trades take place exactly at the time,
            # specified by the timestamp of the record.


            # Determine if one needs to cut away the first record:
            # if the 1st record was removed by the Dates (set by a user),
            # there's no need to remove it later (or adjust the number of
            # transactions by 1 when calculating the total num. of transactions)

            if(!is.null(Dates)) {

                initTxnTime <- index(first(txn))
                attrTZ <- attr(txn,"tzone")
                # Dates = "2002-10-23::2002-10-30"
                timeSpan <- .parseISO8601(Dates, tz = attrTZ) # Thanks to Joshua for suggesting ".parseISO8601()"

                # browser()
                if( initTxnTime >= timeSpan$first.time &&
                    initTxnTime <= timeSpan$last.time ) {
                    if(debugF) print("init transaction is inside the scope")
                    initTxnPresentFlag <- TRUE
                } else {
                    if(debugF) print("init transaction is outside the scope")
                    initTxnPresentFlag <- FALSE
                }

            } else {
                if(debugF) print("init transaction is inside the scope")
                initTxnPresentFlag <- TRUE
            }

            if(!is.null(Dates)) {
                txn   <- txn[Dates]
                posPL <- posPL[Dates]
            }

            dateMin <- min(index(posPL))
            dateMax <- max(index(posPL))
            #---proposed extension-END-OF-SECTION----------------------------- -

            PL.gt0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL  > 0]
            PL.lt0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL  < 0]
            PL.ne0 <- txn$Net.Txn.Realized.PL[txn$Net.Txn.Realized.PL != 0]

            if(length(PL.ne0) == 0)
            {
                # apply.daily will crash
                next
            }

            if(!isTRUE(inclZeroDays)) DailyPL <- apply.daily(PL.ne0,sum)
            else DailyPL <- apply.daily(txn$Net.Txn.Realized.PL,sum)

            AvgDailyPL <- mean(DailyPL)
            MedDailyPL <- median(DailyPL)
            StdDailyPL <- sd(as.numeric(as.vector(DailyPL)))

            switch(use,
                   txns = {
                       #moved above for daily stats for now
                   },
                   trades = {
                       # FIXME: perTradeStats has to support scoped calc's ----
                       trades <- perTradeStatsExt(pname,symbol,tradeDef=tradeDef, Dates=Dates)
                       PL.gt0 <- trades$Net.Trading.PL[trades$Net.Trading.PL  > 0]
                       PL.lt0 <- trades$Net.Trading.PL[trades$Net.Trading.PL  < 0]
                       PL.ne0 <- trades$Net.Trading.PL[trades$Net.Trading.PL != 0]
                   }
            )
            if(!length(PL.ne0)>0)next()

            GrossProfits <- sum(PL.gt0)
            GrossLosses  <- sum(PL.lt0)
            ProfitFactor <- ifelse(GrossLosses == 0, NA, abs(GrossProfits/GrossLosses))

            AvgTradePL <- mean(PL.ne0)
            MedTradePL <- median(PL.ne0)
            StdTradePL <- sd(as.numeric(as.vector(PL.ne0)))
            AnnSharpe  <- ifelse(StdDailyPL == 0, NA, AvgDailyPL/StdDailyPL * sqrt(252))

            #---proposed extension-START-OF-SECTION--------------------------- -
            if(initTxnPresentFlag) {
                NumberOfTxns   <- nrow(txn)-1
            } else {
                NumberOfTxns   <- nrow(txn)
            }
            #---proposed extension-END-OF-SECTION----------------------------- -

            NumberOfTrades <- length(PL.ne0)

            PercentPositive <- (length(PL.gt0)/length(PL.ne0))*100
            PercentNegative <- (length(PL.lt0)/length(PL.ne0))*100

            MaxWin  <- max(txn$Net.Txn.Realized.PL)
            MaxLoss <- min(txn$Net.Txn.Realized.PL)

            AvgWinTrade  <- mean(PL.gt0)
            MedWinTrade  <- median(PL.gt0)
            AvgLossTrade <- mean(PL.lt0)
            MedLossTrade <- median(PL.lt0)

            AvgWinLoss <- ifelse(AvgLossTrade == 0, NA, AvgWinTrade/-AvgLossTrade)
            MedWinLoss <- ifelse(MedLossTrade == 0, NA, MedWinTrade/-MedLossTrade)

            Equity <- cumsum(posPL$Net.Trading.PL)
            if(!nrow(Equity)){
                warning('No Equity rows for',symbol)
                next()
            }
            TotalNetProfit <- last(Equity)
            if(is.na(TotalNetProfit)) {
                warning('TotalNetProfit NA for',symbol)
                next()
            }
            Equity.max       <- cummax(Equity)
            MaxEquity        <- max(Equity)
            MinEquity        <- min(Equity)
            EndEquity        <- last(Equity)
            names(EndEquity) <-'End.Equity'
            if(EndEquity!=TotalNetProfit && last(txn$Pos.Qty)==0) {
                warning('Total Net Profit for',symbol,
                        'from transactions',TotalNetProfit,
                        'and cumulative P&L from the Equity Curve', EndEquity,
                        'do not match. This can happen in long/short portfolios.')
                message('Total Net Profit for',symbol,'from transactions',
                        TotalNetProfit,'and cumulative P&L from the Equity Curve',
                        EndEquity, 'do not match. This can happen in long/short portfolios.')

            }# if we're flat, these numbers should agree
            #TODO we should back out position value if we've got an open position and double check here....

            MaxDrawdown            <- -max(Equity.max - Equity)
            ProfitToMaxDraw  <- ifelse(MaxDrawdown == 0, NA, -TotalNetProfit / MaxDrawdown)
            names(ProfitToMaxDraw) <- 'Profit.To.Max.Draw'

            # TODO: PerfA --> see what must be added to enable the following analytics: ----
            #TODO add skewness, kurtosis, and positive/negative semideviation if PerfA is available.

            #---proposed extension-START-OF-SECTION--------------------------- -
            # calculate extended statistics

            # the 'init' record of txn must be removed if not filtered by Dates
            if(initTxnPresentFlag){
                es <- getExtStats(portfolio=pname, symbol=symbol,
                                  ppl = posPL, trx = txn[-1,],
                                  dateMin=dateMin, dateMax=dateMax,
                                  dates = Dates, interval=Interval)
            } else {
                es <- getExtStats(portfolio=pname, symbol=symbol,
                                  ppl = posPL, trx = txn,
                                  dateMin=dateMin, dateMax=dateMax,
                                  dates = Dates, interval=Interval)
            }

            #---proposed extension-END-OF-SECTION----------------------------- -

            tmpret <- data.frame(Portfolio          = pname,
                                 Symbol             = symbol,
                                 Num.Txns           = NumberOfTxns,
                                 Num.Trades         = NumberOfTrades,
                                 Total.Net.Profit   = TotalNetProfit,
                                 Avg.Trade.PL       = AvgTradePL,
                                 Med.Trade.PL       = MedTradePL,
                                 Largest.Winner     = MaxWin,
                                 Largest.Loser      = MaxLoss,
                                 Gross.Profits      = GrossProfits,
                                 Gross.Losses       = GrossLosses,
                                 Std.Dev.Trade.PL   = StdTradePL,
                                 Percent.Positive   = PercentPositive,
                                 Percent.Negative   = PercentNegative,
                                 Profit.Factor      = ProfitFactor,
                                 Avg.Win.Trade      = AvgWinTrade,
                                 Med.Win.Trade      = MedWinTrade,
                                 Avg.Losing.Trade   = AvgLossTrade,
                                 Med.Losing.Trade   = MedLossTrade,
                                 Avg.Daily.PL       = AvgDailyPL,
                                 Med.Daily.PL       = MedDailyPL,
                                 Std.Dev.Daily.PL   = StdDailyPL,
                                 Ann.Sharpe         = AnnSharpe,
                                 Max.Drawdown       = MaxDrawdown,
                                 Profit.To.Max.Draw = ProfitToMaxDraw,
                                 Avg.WinLoss.Ratio  = AvgWinLoss,
                                 Med.WinLoss.Ratio  = MedWinLoss,
                                 Max.Equity         = MaxEquity,
                                 Min.Equity         = MinEquity,
                                 End.Equity         = EndEquity,

                                 #---proposed extension-START-OF-SECTION------ -
                                 # TODO: 'Bars' might be more properly called "Intervals"
                                 # as the PL might be marked not on every market data record
                                 # but as specified by the 'Interval' argument
                                 # ------------------------------------------- -
                                 # For now, I changed Bars to 'PLRecs' for 'Position PL records'
                                 # to be more consistent
                                 Max.Consec.Win.Trades  = es$Max.Consec.Winning.Trades,
                                 Max.Consec.Los.Trades  = es$Max.Consec.Losing.Trades,
                                 Avg.PLRecs.All.Trades  = es$Avg.Bars.In.Total.Trades,
                                 Avg.PLRecs.Win.Trades  = es$Avg.Bars.In.Winning.Trades,
                                 Avg.PLRecs.Los.Trades  = es$Avg.Bars.In.Losing.Trades,
                                 Max.PLRecs.Flat.Period = es$Max.Bars.Flat.Period,
                                 Percent.Time.In.Market = es$Percent.Time.In.Market,
                                 RINA.Index             = es$RINA.Index,
                                 Date.Min               = dateMin,
                                 Date.Max               = dateMax
                                 #---proposed extension-END-OF-SECTION-------- -
            )
            rownames(tmpret) <- symbol
            ret              <- rbind(ret,tmpret)
        } # end symbol loop
    } # end portfolio loop
    return(ret)
}

# Check with this list of stats (mentioned in Tomasini & J)
# '+' - marks existing stats
# 'O' - extended stats
# ---------------------------------------------- -
# + # Test Period from
# + # Test period until
# + # Total Net Profit
# + # Gross Profit
# + # Gross Loss
# + # Profit Factor
# + # Total Number of Trades
#   # Total Number of Long Trades
#   # Total Number of Short Trades
# + # Percent Profitable
#   # Winning Trades
#   # Losing Trades
#   # Even Trades
# + # Avg. Trade Net Profit
# + # Avg. Winning Trade
# + # Avg. Losing Trade
#   # Ratio Avg. Win:Avg. Loss
# + # Largest Winning Trade
# + # Largest Losing Trade
# O # Max. Consecutive Winning Trades
# O # Max. Consecutive Losing Trades
# O # Avg. Bars in Total Trades
# O # Avg. Bars in Winning Trades
# O # Avg. Bars in Losing Trades
#   # Annual Rate of Return
#   # Avg. Monthly Return
#   # Std. Deviation of Monthly Return
#   # Return Retracement Ratio
#   # RINA Index
# + # Sharpe Ratio
#   # K-Ratio
#   # Trading Period (Length of the period in years/months/days)
# + # Percent of Time in the Market
#   # Time in the Market
# O # Longest Flat Period
#   # Max. Equity Run-up
#   # Date of Max. Equity Run-up
#   # Max. Equity Run-up as % of Initial Capital
# ? # Max. Drawdown (Intra-day Peak to Valley)
#   # Date of Max. Drawdown
#   # Total Slippage and Commission
#   # Longest Period Out (days/bars/ticks)
#   # Average Time Between Trades (bars/ticks)
#   # Average Time to Reach New High (bars/ticks)
# ---------------------------------------------- -

# RINA Index = (Net Profit - Net Profit in Outliers)/(Average Drawdown * Percent Time in the Market)
# Reference: http://signaltradinggroup.com/wp-content/DCSArticles/TSperform.pdf
# No clear definition of "Outliers" was given in the reference above.
# (Some sources omit "Net Profit in Outliers" altogether,
# e.g.: https://inovancetech.com/strategyEvaluation.html)
#
# RINA Index: Definition provided in
# Jaekle & Tomasini: A new approach to system development and portfolio optimisation (ISBN 978-1-905641-79-6)
# Paragraph 2.4: Evaluation of a trading system:
# "the reward-risk ratio per one unit of time" ... "compares the select net
# profit (net profit minus the positive and negative outlier trades, that is
# minus the abnormal trades that overcome the three standard deviation limit
# away from the average) divided by the average drawdown and again divided by
# the percentage of time in the market indicator."

# K-Ratio = (Slope of Log VAMI Regression line) / ((Standard error of the slope)*(Number of period in the Log VAMI))
# Reference: http://signaltradinggroup.com/wp-content/DCSArticles/TSperform.pdf
# VAMI is a monthly plot of the progress of a hypothetical $1000 initial investment.
# Using any log base will result in the same final value.
# The denominator of the K-Ratio is multiplied by the square root of
# observations to normalize the measure across different time frames.


## FIXME: ----------------------------------------------------------------------
# when a portfolio is updated not on every timestamp, but on intervals larger
# than intervals b/n market data timestamps, Position PL records are formed out
# of a 'union' of records based on the Interval argument and records that
# correspond to records within transactions table. CHECK THIS IS CORRECT!!!
# so when I calculate such statistics as "% time in the market" based on PosPL
# number of records, the statistics will be slightly distorted (for portfolios
# marked on larger intervals than intervals b/n mkt data records)
# ------------------ -
# time in the market should be based on actual timestamps to be correct, so the
# statement above should be applied to statistics based on the number of PosPL
# records in general
# at the same time, those records not falling on Interval
# endpoints could simply be removed from PosPL and the statistics will not be
# distorted by those "txn" table records
## --------------------------------------------------------------------------- -

#' Filters position PL records that correspond to set intervals exactly;
#'        removes all the 'extra' records, including those of transactions
#'
#' @param ct environment with context variables, which must include the
#'        following: portfolio, symbol, ppl (position PL), dates,
#'        dargs(expanded '...')
#' @export
intervalFilteredPosPL <- function(ct, interval=NULL)
{
    dargs <- ct$dargs
    Interval <- interval
    # reworked code from blotter:::.updatePosPL

    # TODO: consider moving it out into some 'utility' function
    # if no date is specified, get all available dates
    if(!missing(interval) && !is.null(interval)) {
        # dargs <- list(...) # for now, I don't need args anywhere else
        if(!is.null(dargs$env)) {env <- dargs$env} else env=.GlobalEnv
        if(!is.null(dargs$prefer)) {prefer<-dargs$prefer} else prefer=NULL
        # if(!is.null(dargs$symbol)) {symbol<-dargs$symbol} else symbol=NULL
        # prices=getPrice(get(Symbol, pos=env), symbol=symbol, prefer=prefer)[,1]
        prices=getPrice(get(symbol, pos=env), prefer=prefer)[,1]
        ep_args <- blotter:::.parse_interval(interval)
        ## "time zero" for endpoints is always linked to the beginning of the
        ## available price time series. Therefore, "endpoints" may be different
        ## for the same study depending on the loaded price data This is a good
        ## approach as it relieves the user of the burden to keep an extra
        ## variable for the beginning of the time series across other functions
        ## of the QS framework
        prices <- prices[endpoints(prices, on=ep_args$on, k=ep_args$k)]
    }
    # browser()

    if(is.null(dates)) {
        dates = index(prices)
        # Covert to POSIXct w/same TZ as portfolio object
        if(any(indexClass(prices) %in% c("Date","yearmon","yearqtr"))) {
            # portfTZ <- indexTZ(Portfolio$symbols[[Symbol]]$txn)
            portfTZ <- indexTZ(trx)
            dates <- as.POSIXct(as.character(as.Date(dates)), tz=portfTZ)
        }
    } else if(!is.timeBased(dates)) {
        dates<- if(is.na(.parseISO8601(dates)$first.time) ||
                   .parseISO8601(dates)$first.time < as.POSIXct(first(index(prices)))) {
            index(prices[paste('/',.parseISO8601(dates)$last.time,sep='')])
        } else {
            index(prices[dates])
        }
    }
    # line up Prices dates with Dates set/index/span passed in.
    startDate = first(dates)
    endDate   = last(dates)
    if(is.na(endDate)) endDate<-NULL
    dateRange = paste(startDate,endDate,sep='::')

    #subset Prices by dateRange too...
    prices<-prices[dateRange]

    # Leave the last duplicated position PL record as a duplicate is usually
    # an exit and an entry. So calculating
    duplicateRecords <- duplicated(.index(ppl),fromLast = TRUE)
    uniquePosPLRecords <- ppl[!duplicateRecords]

    # ct$intFiltPPL <- merge(prices, uniquePosPLRecords, join = 'left')
    # ct$outVar <- "intFiltPPL"
    # ct
    intFiltPPL <- merge(prices, uniquePosPLRecords, join = 'left')
    intFiltPPL
}

# Function getExtStats() calculates additional statistics
# Arguments:
#     ppl - 'Position PL' data frame w/o the initialization 'record' ('row')
#     trx - 'transactions' data frame w/o the initialization 'record' ('row')
# Notes:
# 1. if the last record of transactions table is not 'completing' a trade
#    the last row of the aggregated ppl table is removed before "cbinding"
# 2. the first row of trx table must not contain the 'empty' ('init'/'0') data
#    i.e. it must be removed before calling getExtStats
getExtStats <- function(portfolio, symbol,
                        ppl, trx,
                        dateMin, dateMax,
                        dates=NULL,
                        interval, ...)
{ # @author cloudcello

    # create an environment to pass 'context' variables by 'reference'
    ctx           <- new.env()
    ctx$portfolio <- portfolio
    ctx$symbol    <- symbol
    ctx$ppl       <- ppl
    ctx$dates     <- dates
    ctx$dargs     <- list(...)
    ### ---------------------------------------------------------------------- -

    # TODO: a proper table of 'trades' is needed in the portfolio
    # such a table shall contain trades as defined in the argument to tradeStats
    # 3 methods to define a trade:
    #            "flat-to-flat"
    #            "position-reducing"
    #            "logically-tagged-open-closed" (algorithm specific)
    # (source: http://quant.stackexchange.com/questions/9213/how-do-order-management-matching-systems-match-allocate-orders-and-filled-price)
    # ---
    # Additional reference: https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/quantstrat/sandbox/backtest_musings/strat_dev_process.pdf?root=blotter
    # methods to calculate round-trip trades:
    # 1. FIFO
    # 2. tax lots
    # 3. flat to flat
    # 4. flat to reduced
    # 5. increased to reduced (a superior alternative to FIFO) & avg.cost
    # ---
    # Use blotter::perTradeStats() to build the table of trades
    # ---

    # View(ppl)
    # View(trx)
    print(dateMin)
    print(dateMax)
    print(dateMax - dateMin)

    o <- list() # final output

    # FIXME: this value is not used anywhere atm --> 2b fixed after this commit
    intFiltPPL <- intervalFilteredPosPL(ctx,interval = interval)

    ### ---------------------------------------------------------------------- -

    # FIXME: check why posPL$Pos.Qty field when used to determine trades ----
    # causes tests to fail
    pplFlags <- vector(nrow(ppl$Pos.Qty), mode = "integer")
    # pplFlags[ppl$Pos.Qty > 0] <- 1 # (+1)
    pplFlags[ppl$Pos.Avg.Cost > 0] <- 1 # (+1)

    spans <- rle(pplFlags)
    spans.df <- data.frame(spans$lengths,spans$values)

    # run-length-encoded PosPL records 'in the market'
    rleInMkt <- spans.df[spans.df$spans.values==1,]

    # use Net.Txn.Realized.PL from this table to get Win/Los status
    # trxWinLos <- trx[trx$Pos.Qty==0]$Net.Txn.Realized.PL
    trxWinLos <- trx[trx$Pos.Avg.Cost==0]$Net.Txn.Realized.PL

    trxWinLosFlag <- vector(nrow(trxWinLos), mode = "integer")
    trxWinLosFlag[trxWinLos<0] <- -1 # (-1)
    trxWinLosFlag[trxWinLos>0] <-  1 # (+1)

    # check the last transaction status -- does it complete the trade ?
    # lastTradeIsIncomplete.Flag <- as.logical(last(trx)$Pos.Qty!=0)
    lastTradeIsIncomplete.Flag <- as.logical(last(trx)$Pos.Avg.Cost!=0)
    # When stats are 'scoped', trades whose 'beginning' transactions
    # are out of scope are still counted as completed trades
    if(lastTradeIsIncomplete.Flag) {
        # remove 'in the market' data for an incomplete trade
        # (the last transaction which makes up that incomplete trade)
        rleInMktCompletedTradesOnly <- rleInMkt[-nrow(rleInMkt),]
    } else {
        rleInMktCompletedTradesOnly <- rleInMkt
    }

    if(0) browser()

    # completed trades only !
    rleInMktSigned <- cbind(rleInMktCompletedTradesOnly, trxWinLosFlag)

    consecWinLosTrades    <- rle(rleInMktSigned$trxWinLosFlag)
    consecWinLosTrades.df <- data.frame(lengths=consecWinLosTrades$lengths,
                                        values=consecWinLosTrades$values)


    # "Max. Consecutive Winning Trades"
    tmp0.df <- consecWinLosTrades.df[consecWinLosTrades.df$values==1,]
    if(nrow(tmp0.df)>0) {
        o$Max.Consec.Winning.Trades <- max(tmp0.df$lengths)
    } else {
        o$Max.Consec.Winning.Trades <- NA
    }

    # "Max. Consecutive Losing Trades"
    tmp1.df <- consecWinLosTrades.df[consecWinLosTrades.df$values==-1,]
    if(nrow(tmp1.df)>0) {
        o$Max.Consec.Losing.Trades <- max(tmp1.df$lengths)
    } else {
        o$Max.Consec.Losing.Trades <- NA
    }

    # "Avg. Bars in Total Trades"
    o$Avg.Bars.In.Total.Trades <- mean(rleInMktSigned$spans.lengths)

    # "Avg. Bars in Winning Trades"
    o$Avg.Bars.In.Winning.Trades <- mean(
        rleInMktSigned$spans.lengths[rleInMktSigned$trxWinLosFlag==+1])

    # "Avg. Bars in Losing Trades"
    o$Avg.Bars.In.Losing.Trades <- mean(
        rleInMktSigned$spans.lengths[rleInMktSigned$trxWinLosFlag==-1])

    # "Longest Flat Period" (in Bars)
    o$Max.Bars.Flat.Period <- max(
        spans.df[spans.df$spans.values==0,]$spans.lengths)

    # "Percent of Time in the Market"
    # Note: may include an incomplete trade at the end of date/time scope
    # Reference for the Percent.Time.In.Market (TODO: add to 'help')
    #     the statistic assumes that market data for the symbol includes
    #     all and only time periods during which the market was open and
    #     that data records (quotes) are separated by equal time intervals
    Bars.In.Market     <- sum(spans.df[spans.df$spans.values!=0,]$spans.lengths)
    # Bars.Not.In.Market <- sum(spans.df[spans.df$spans.values==0,]$spans.lengths)
    Percent.Time.In.Market <- 100 * Bars.In.Market / length(pplFlags)
    o$Percent.Time.In.Market <- Percent.Time.In.Market

    # ------------------------------------------------------------------------ -
    # FIXME: use code from .updatePosPL() to clean up posPL from ----
    # unneeded records and ONLY then calculate Percent.Time.In.Market
    # ------------------------------------------------------------------------ -


    # RINA Index (NEEDS CHECKING !!!)
    # RINA Index = (Net Profit - Net Profit in Outliers)/(Average Drawdown * Percent Time in the Market)
    # RINA Idx Numerator:
    stDevX3 <- StdDev(trxWinLos)[1] * 3
    netProfitInOutliers <- sum( trxWinLos[trxWinLos > +stDevX3],
                                trxWinLos[trxWinLos < -stDevX3])
    RINAIdxNumerator <- sum(trxWinLos) - netProfitInOutliers
    # RINA Idx Denominator:
    Equity       <- cumsum(ppl$Net.Trading.PL) # copied from tradeStats
    Equity.max   <- cummax(Equity)             # copied from tradeStats
    Avg.Drawdown <- mean(Equity.max - Equity)
    RINAIdxDenominator <- Avg.Drawdown * (Percent.Time.In.Market/100)
    # finally, the index:
    o$RINA.Index <- RINAIdxNumerator / RINAIdxDenominator

    ##------------------------------------------------------------------------ -
    ## Timestamp-based statistics -- to provide time referenced (as opposed
    ## to market data availability referenced) statistics
    ##
    ## based on blotter::perTradeStats
    if(0) { # won't work at the moment because there is no easy way to
            # account for weekends / holidays, so removal of posPL is easier

        pts <- perTradeStatsExt(Portfolio=portfolio, Symbol = symbol, Dates=dates)

        # View(pts)

        browser()

        print("perTradeStats-based statistics: ...work in progress...")
    }
    #------------------------------------------------------------------------- -


    # end of function
    o
}

# sandbox -------------------------------------------------------------------- -
if(0) {
    # str(pts)
    # pts[,1]
    # pts$Start
    # pts$End
    # shift the End up to get the reverse set of timespans with no activity
    starts <- c(dateMin,pts$End)
    ends <- c(pts$Start,dateMax)
    str(starts)
    str(pts$Start)
    outOfMarketTime <- as.data.frame(list(Start=starts,End=ends))
    # add period lengths
    tDiff <- outOfMarketTime$End - outOfMarketTime$Start
    outOfMarketTime <- cbind(outOfMarketTime,tDiff)
    str(outOfMarketTime)
}
