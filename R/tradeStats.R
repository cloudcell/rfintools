# Original Code Source: QuantStrat (blotter) R package

# See sections marked with the following 'tag lines'
#---proposed extension-START-OF-SECTION--------------------------- -
#---proposed extension-END-OF-SECTION----------------------------- -

# Extensions proposed by 'cloudcell':
#  1. date/time filter
#  2. percent.time.in.market indicator solution

tradeStats <- function(Portfolios, Symbols, use=c('txns','trades'),
                       tradeDef='flat.to.flat',inclZeroDays=FALSE, Dates=NULL)
{
    ret <- NULL
    use <- use[1] #use the first(default) value only if user hasn't specified
    tradeDef <- tradeDef[1]
    for (Portfolio in Portfolios){
        pname <- Portfolio
        Portfolio<-.getPortfolio(pname)

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
            # However, this solution is not applicable for tick data.
            # So the current solution in this file is more straightforward.
            #
            # Also, an easier way to accept the current solution is to use an
            # assumption that all the trades take place exactly at the time,
            # specified by the timestamp of the record.

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
                       trades <- perTradeStats(pname,symbol,tradeDef=tradeDef)
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

            NumberOfTxns   <- nrow(txn)-1
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
                warning('Total Net Profit for',symbol,'from transactions',TotalNetProfit,'and cumulative P&L from the Equity Curve', EndEquity, 'do not match. This can happen in long/short portfolios.')
                message('Total Net Profit for',symbol,'from transactions',TotalNetProfit,'and cumulative P&L from the Equity Curve', EndEquity, 'do not match. This can happen in long/short portfolios.')

            }# if we're flat, these numbers should agree
            #TODO we should back out position value if we've got an open position and double check here....

            MaxDrawdown            <- -max(Equity.max - Equity)
            ProfitToMaxDraw  <- ifelse(MaxDrawdown == 0, NA, -TotalNetProfit / MaxDrawdown)
            names(ProfitToMaxDraw) <- 'Profit.To.Max.Draw'

            #TODO add skewness, kurtosis, and positive/negative semideviation if PerfA is available.

            #---proposed extension-START-OF-SECTION--------------------------- -
            # calculate extended statistics
            # attn!: the top record of txn must be removed !
            es <- getExtStats(ppl = posPL, trx = txn[-1,])
            #---proposed extension-END-OF-SECTION----------------------------- -

            tmpret <- data.frame(Portfolio=pname,
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
                                 Max.Consec.Winning.Trades  = es$Max.Consec.Winning.Trades,
                                 Max.Consec.Losing.Trades   = es$Max.Consec.Losing.Trades,
                                 Avg.Bars.In.Total.Trades   = es$Avg.Bars.In.Total.Trades,
                                 Avg.Bars.In.Winning.Trades = es$Avg.Bars.In.Winning.Trades,
                                 Avg.Bars.In.Losing.Trades  = es$Avg.Bars.In.Losing.Trades,
                                 Max.Bars.Flat.Period       = es$Max.Bars.Flat.Period,
                                 Percent.Time.In.Market     = es$Percent.Time.In.Market,
                                 RINA.Index                 = es$RINA.Index,

                                 Date.Min                   = dateMin,
                                 Date.Max                   = dateMax
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
# "the reward-risk ratio per one unit of time" ... "compares the “select net
# profit” (net profit minus the positive and negative outlier trades, that is
# minus the abnormal trades that overcome the three standard deviation limit
# away from the average) divided by the average drawdown and again divided by
# the percentage of time in the market indicator."

# K-Ratio = (Slope of Log VAMI Regression line) / ((Standard error of the slope)*(Number of period in the Log VAMI))
# Reference: http://signaltradinggroup.com/wp-content/DCSArticles/TSperform.pdf
# VAMI is a monthly plot of the progress of a hypothetical $1000 initial investment.
# Using any log base will result in the same final value.
# The denominator of the K-Ratio is multiplied by the square root of
# observations to normalize the measure across different time frames.


# Function getExtStats() calculates additional statistics
# Arguments:
#     ppl - 'Position PL' data frame w/o the initialization 'record' ('row')
#     trx - 'transactions' data frame w/o the initialization 'record' ('row')
# Notes:
# 1. if the last record of transactions table is not 'completing' a trade
#    the last row of the aggregated ppl table is removed before "cbinding"
# 2. the first row of trx table must not contain the 'empty' ('init'/'0') data
#    i.e. it must be removed before calling getExtStats
getExtStats <- function(ppl,trx)
{ # @author cloudcello
    if(0) browser()
    o <- list()

    pplFlags <- vector(nrow(ppl$Pos.Avg.Cost), mode = "integer")
    pplFlags[ppl$Pos.Avg.Cost > 0] <- 1 # (+1)

    spans <- rle(pplFlags)
    spans.df <- data.frame(spans$lengths,spans$values)

    # run-length-encoded PosPL records 'in the market'
    rleInMkt <- spans.df[spans.df$spans.values==1,]

    # use Net.Txn.Realized.PL from this table to get Win/Los status
    trxWinLos <- trx[trx$Pos.Avg.Cost==0]$Net.Txn.Realized.PL

    trxWinLosFlag <- vector(nrow(trxWinLos), mode = "integer")
    trxWinLosFlag[trxWinLos<0] <- -1 # (-1)
    trxWinLosFlag[trxWinLos>0] <-  1 # (+1)

    # check the last transaction status -- does it complete the trade ?
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

    # completed trades only !
    rleInMktSigned <- cbind(rleInMktCompletedTradesOnly, trxWinLosFlag)

    consecWinLosTrades    <- rle(rleInMktSigned$trxWinLosFlag)
    consecWinLosTrades.df <- data.frame(lengths=consecWinLosTrades$lengths,
                                        values=consecWinLosTrades$values)

    # "Max. Consecutive Winning Trades"
    tmp.df <- consecWinLosTrades.df[consecWinLosTrades.df$values==1,]
    Max.Consec.Winning.Trades   <- max(tmp.df$lengths)
    o$Max.Consec.Winning.Trades <- Max.Consec.Winning.Trades

    # "Max. Consecutive Losing Trades"
    tmp.df <- consecWinLosTrades.df[consecWinLosTrades.df$values==-1,]
    Max.Consec.Losing.Trades   <- max(tmp.df$lengths)
    o$Max.Consec.Losing.Trades <- Max.Consec.Losing.Trades

    # "Avg. Bars in Total Trades"
    Avg.Bars.In.Total.Trades   <- mean(rleInMktSigned$spans.lengths)
    o$Avg.Bars.In.Total.Trades <- Avg.Bars.In.Total.Trades

    # "Avg. Bars in Winning Trades"
    Avg.Bars.In.Winning.Trades   <- mean(
        rleInMktSigned$spans.lengths[rleInMktSigned$trxWinLosFlag==+1])
    o$Avg.Bars.In.Winning.Trades <- Avg.Bars.In.Winning.Trades

    # "Avg. Bars in Losing Trades"
    Avg.Bars.In.Losing.Trades   <- mean(
        rleInMktSigned$spans.lengths[rleInMktSigned$trxWinLosFlag==-1])
    o$Avg.Bars.In.Losing.Trades <- Avg.Bars.In.Losing.Trades

    # "Longest Flat Period" (in Bars)
    Max.Bars.Flat.Period   <- max(
        spans.df[spans.df$spans.values==0,]$spans.lengths)
    o$Max.Bars.Flat.Period <- Max.Bars.Flat.Period

    # "Percent of Time in the Market"
    # Note: may include an incomplete trade at the end of time scope
    # Reference for the Percent.Time.In.Market (TODO: add to 'help')
    #     the statistic assumes that market data for the symbol includes
    #     all and only time periods during which the market was open and
    #     that data records (quotes) are separated by equal time intervals
    Bars.In.Market     <- sum(spans.df[spans.df$spans.values!=0,]$spans.lengths)
    # Bars.Not.In.Market <- sum(spans.df[spans.df$spans.values==0,]$spans.lengths)
    Percent.Time.In.Market <- 100 * Bars.In.Market / length(pplFlags)
    o$Percent.Time.In.Market <- Percent.Time.In.Market

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
    RINA.Index <- RINAIdxNumerator / RINAIdxDenominator
    o$RINA.Index <- RINA.Index

    # end of function
    o
}



