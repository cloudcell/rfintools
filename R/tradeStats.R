# Source: QuantStrat (blotter) R package
# This repository is a temporary location for this function, until I make a patch

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
            # posPL <- posPL[-1,] # shifted after 'Scope' is calculated
            
            if(isNull(Dates)) { posPL <- posPL[-1,] }
            
            # Removes info in the range '( t(-1); t(0) ]'
            # The first record (technically) includes 'information' 
            # external to the scope.
            # E.g. a record timestamped "2002-02-21 00:00"
            #      for 30 minute-per-bar data will include events from
            #      "2002-02-20 23:30" until "2002-02-21 00:00"
            #      and, therefore, must be excluded.
            
            
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
                message("subsetting data")
                txn   <- txn[Dates]
                posPL <- posPL[Dates]
            }
            
            # prepare "Scope" for output (named "Scope" to avoid confusion with 
            # the technical "Date")
            
            # The first actual record will be removed as it drags info
            # from the period external to the scope (see the comment below)
            dateMin <- min(index(posPL)) 
            dateMax <- max(index(posPL))
            scope <- paste0(dateMin, "::", dateMax)
            
            # TODO: * decide on whether to add a warning if the first date is 
            #         earlier than the date of the first record in posPL
            #       * alternatively, add a note in the 'help' about the first 
            #         record being thrown out
            
            # Percent.Time.In.Market
            # Disclaimer for the %-time-in-the-market 
            #     the statistic assumes that market data for the symbol includes 
            #     all and only time periods during which the market was open and 
            #     that data records (quotes) are at equal time intervals
            #     TODO: add this to some reference in the help file(?)
            
            posPLRecNbr      <- nrow(posPL)
            posPLRecInMktNbr <- nrow(posPL[posPL$Pos.Avg.Cost != 0])
            if(posPLRecNbr == 0) { next }
            
            Percent.Time.In.Market <- 100 * posPLRecInMktNbr / posPLRecNbr
            
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
                                 Percent.Time.In.Market  = Percent.Time.In.Market,
                                 Scope              = scope
                                 # TODO: there might be a problem with throwing 
                                 # out the first record of PosPL but not including
                                 # the first "next bar" outside the scope.
                                 # i.e. if Dates=='2002-10-23', the scope
                                 # will return '2002-10-23::2002-10-23 23:30:00'
                                 # i.e. we 'missed' the data in the last 30 
                                 # minutes of the day!
                                 #
                                 # TODO: add the first record following last 
                                 # record of the 'scope' 
                                 # ---OR--- to stop throwing
                                 # away the first record & make a note of that
                                 # in the reference
                                 #---proposed extension-END-OF-SECTION-------- -
                                 )
            rownames(tmpret) <- symbol             
            ret              <- rbind(ret,tmpret)
        } # end symbol loop
    } # end portfolio loop
    return(ret)
}
