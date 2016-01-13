# Source: QuantStrat (blotter) R package
# This repository is a temporary location for this function, until
# I make a patch with the minimum number of lines to be changed

# See sections marked with the following 'tag lines'
#---proposed extension-START-OF-SECTION--------------------------- -
#---proposed extension-END-OF-SECTION----------------------------- -

# Extensions proposed by 'cloudcell':
#  1. date/time filter
#  2. percent.time.in.market indicator solution

tradeStats <- function(Portfolios, Symbols ,use=c('txns','trades'), tradeDef='flat.to.flat',inclZeroDays=FALSE, Dates=NULL)
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
            # Simply removes the initial record ( not to remove info in the range '( t(-1); t(0) ]' )
            posPL <- posPL[-1,] # no matter whether the stats are 'scoped' or not, the first record is removed
            
            # if(is.null(Dates)) { posPL <- posPL[-1,] }

            # An alternative solution, one might want to
            # remove info in the range '( t(-1); t(0) ]'
            # The first record (technically) includes 'information'
            # external to the scope.
            # E.g. a record timestamped "2002-02-21 00:00"
            #      for 30 minute-per-bar data will include events from
            #      "2002-02-20 23:30" until "2002-02-21 00:00"
            #      and, therefore, must be excluded.
            # However, this solution is not applicable for tick data.
            # So the current solution in this file is more straightforward.

            # Comments:
            # * while there is a 'TODO' for implementing a similar date
            #   'filter' in the getPortfolio function, I propose to implement
            #   a date filter in tradeStats() until all the nuances of
            #   proper performance attribution to time periods are clear.
            #   Developing date subsetting on portfolio level at this point
            #   would be mixing two tasks into one.
            #
            # * Besides, if %-based statistics are to be implemented,
            #   having data for period T-1 might be required, which will
            #   be harder to implement if there is no straightforward access
            #   to the period that is out of scope set by "Dates."
            #
            # * getPortfolio() also returns orderbooks
            #   a record in an orderbook may cover multiple time
            #   periods. Therefore, to be able to subset the object
            #   of type 'Portfolio' one must first resolve the issue with
            #   splitting records of orderbooks.
            #
            # TODO: Decide whether the output should include the scope over
            #       which tradeStats have been calculated
            #       (a field named "Scope" or "Dates")

            if(!is.null(Dates)) {
                # message("subsetting data")
                txn   <- txn[Dates]
                posPL <- posPL[Dates]
            }

            # prepare "Scope" for output (named "Scope" to avoid confusion with
            # the technical "Date")

            # The first actual record will be removed as it drags info
            # from the period external to the scope (see the comment below)
            dateMin <- min(index(posPL))
            dateMax <- max(index(posPL))
            # scope <- paste0(dateMin, "::", dateMax)

            # TODO: * decide on whether to add a warning if the first date is
            #         earlier than the date of the first record in posPL
            #       * alternatively, add a note in the 'help' about the first
            #         record being thrown out

            # Percent.Time.In.Market
            # Reference for the Percent.Time.In.Market
            #     the statistic assumes that market data for the symbol includes
            #     all and only time periods during which the market was open and
            #     that data records (quotes) are at equal time intervals
            #     TODO: add this to some reference in the help file(?)

            # -- migrating extended statistics into an external function --
            # posPLRecNbr      <- nrow(posPL)
            # posPLRecInMktNbr <- nrow(posPL[posPL$Pos.Avg.Cost != 0])
            # if(posPLRecNbr == 0) { next }
            # 
            # Percent.Time.In.Market <- 100 * posPLRecInMktNbr / posPLRecNbr

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
                                 Max.Consec.Winning.Trades  = es$Max.Consec.Winning.Trades  ,
                                 Max.Consec.Losing.Trades   = es$Max.Consec.Losing.Trades   ,
                                 Avg.Bars.In.Total.Trades   = es$Avg.Bars.In.Total.Trades   ,
                                 Avg.Bars.In.Winning.Trades = es$Avg.Bars.In.Winning.Trades ,
                                 Avg.Bars.In.Losing.Trades  = es$Avg.Bars.In.Losing.Trades  ,
                                 Max.Bars.Flat.Period       = es$Max.Bars.Flat.Period       ,
                                 Percent.Time.In.Market     = es$Percent.Time.In.Market     ,
                                 
                                 Date.Min           = dateMin,
                                 Date.Max           = dateMax
                                 #---proposed extension-END-OF-SECTION-------- -
                                 )
            rownames(tmpret) <- symbol
            ret              <- rbind(ret,tmpret)
        } # end symbol loop
    } # end portfolio loop
    return(ret)
}

# Check against this list of stats (mentioned in Tomasini & J)
# ------------------------------------------
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
#   # ------------------------------------------
#   # longest period out (days/bars/ticks)
#   # average time between trades (bars/ticks)
#   # average time to reach new high (bars/ticks)
#   # 

# Notes:
# 1. if the last record of transactions table is not 'completing' a trade
#    the last row of the aggregated ppl table is removed before "cbinding"
# 2. the first row of trx table must not contain the 'empty' ('init'/'0') data
#    i.e. it must be removed before calling getExtStats
getExtStats <- function(ppl,trx)
{
    o <- list()
    
    if(0) {
        p <- getPortfolio("forex")
        str(p)
        ppl <- p$symbols$GBPUSD$posPL
        trx <- p$symbols$GBPUSD$txn
        nrow(trx)
    }
    
    pplFlags <- vector(nrow(ppl$Pos.Avg.Cost), mode = "integer")
    pplFlags[ppl$Pos.Avg.Cost > 0] <- (+1)
    # pplFlags[ppl$Pos.Avg.Cost < 0] <- (-1)
    
    spans <- rle(pplFlags)
    spans.df <- data.frame(spans$lengths,spans$values)
    # spans$lengths
    
    rleInMkt <- spans.df[spans.df$spans.values==1,] # run-length-encoded records 'in the market'
    
    trxWinLos <- trx[trx$Pos.Avg.Cost==0]$Net.Txn.Realized.PL # use Net.Txn.Realized.PL from this table to get Win/Los status
    # lastTradeIsIncomplete.Flag <- if(last(trx)$Pos.Avg.Cost!=0) {TRUE} else {FALSE} # check the last transaction status
    lastTradeIsIncomplete.Flag <- as.logical(last(trx)$Pos.Avg.Cost!=0) # check the last transaction status
    
    trxWinLosFlag <- vector(nrow(trxWinLos), mode = "integer")
    trxWinLosFlag[trxWinLos<0] <- -1
    trxWinLosFlag[trxWinLos>0] <- +1
    # trxWinLosFlag
    
    # When stats are 'scoped', trades whose 'beginning' transactions
    # are out of scope are still counted as completed trades
    if(lastTradeIsIncomplete.Flag) {
        rleInMktCompletedTradesOnly <- rleInMkt[-nrow(rleInMkt),]
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
    # Note: may include an incomplete trade at the end
    Bars.In.Market     <- sum(spans.df[spans.df$spans.values!=0,]$spans.lengths)
    Bars.Not.In.Market <- sum(spans.df[spans.df$spans.values==0,]$spans.lengths)
    Percent.Time.In.Market <- 100 * Bars.In.Market / length(pplFlags)
    o$Percent.Time.In.Market <- Percent.Time.In.Market
    
    # end of function
    o
}




