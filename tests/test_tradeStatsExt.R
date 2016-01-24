# reference on testing packages:
# http://bioconductor.org/developers/unitTesting-guidelines/

# To start using testing packages quickly, here is
# the bare minimum: http://www.johnmyleswhite.com/notebook/2010/08/17/unit-testing-in-r-the-bare-minimum/

# The following test function was borrowed from the package 'rredis'
# checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

# require(RUnit)
require(blotter)
require(quantstrat)
require(rfintools)

options(digits=22) # default == 7
# ?Sys.info()
Sys.timezone()
Sys.setenv(TZ='UTC')

checkEquals <- function(x, y) if(!isTRUE(all.equal(x, y, check.attributes=FALSE))) stop()

# dir=system.file('data',package='quantstrat')
dir=system.file('extdata',package='rfintools')
load(file = paste0(dir,"/","test_tradeStats.RData"))

put.portfolio(portf2,portfolio.st = "forex")

if(0) {
    getPortfolio("forex")$symbols$GBPUSD$txn
    getPortfolio("forex")$symbols$GBPUSD$posPL
}

# full data ------------------------------------------------------------------ -
cat("running test on the full data\n")

ts <- tradeStatsExt("forex","GBPUSD", debugF = TRUE)

attr(ts$Date.Min, "tzone") <- "UTC"
attr(ts$Date.Max, "tzone") <- "UTC"
str(ts)

# checkEquals( ts'ata.frame':	1 obs. of  40 variables:
checkEquals( levels(ts$Portfolio)         , "forex" )
checkEquals( levels(ts$Symbol)            , "GBPUSD" )
checkEquals( ts$Num.Txns                  , 24 )
checkEquals( ts$Num.Trades                , 12 )
checkEquals( ts$Net.Trading.PL            , -492 )
checkEquals( ts$Avg.Trade.PL              , -41 )
checkEquals( ts$Med.Trade.PL              , -136 )
checkEquals( ts$Largest.Winner            , 414 )
checkEquals( ts$Largest.Loser             , -396 )
checkEquals( ts$Gross.Profits             , 1296 )
checkEquals( ts$Gross.Losses              , -1788 )
checkEquals( ts$Std.Dev.Trade.PL          , 285.3865 )
checkEquals( ts$Percent.Positive          , 33.333333333333329 )
checkEquals( ts$Percent.Negative          , 66.666666666666657)
checkEquals( ts$Profit.Factor             , 0.72483221476512438 )
checkEquals( ts$Avg.Win.Trade             , 324 )
checkEquals( ts$Med.Win.Trade             , 359 )
checkEquals( ts$Avg.Losing.Trade          , -223.5 )
checkEquals( ts$Med.Losing.Trade          , -241 )
checkEquals( ts$Avg.Daily.PL              , -61.5 )
checkEquals( ts$Med.Daily.PL              , -121 )
checkEquals( ts$Std.Dev.Daily.PL          , 377.67975852566377 )
checkEquals( ts$Ann.Sharpe                , -2.5849471986368129 )
checkEquals( ts$Max.Drawdown              , -1424 )
checkEquals( ts$Profit.To.Max.Draw        , -0.3455056179775281 )
checkEquals( ts$Avg.WinLoss.Ratio         , 1.4496644295302488 )
checkEquals( ts$Med.WinLoss.Ratio         , 1.4896265560166377 )
checkEquals( ts$Max.Equity                , 400 )
checkEquals( ts$Min.Equity                , -1024 )
checkEquals( ts$End.Equity                , -492 )

checkEquals( ts$Max.Consec.Win.Trades     , 2 )
checkEquals( ts$Max.Consec.Los.Trades     , 4 )
checkEquals( ts$Avg.PLRecs.All.Trades     , 22.58333333 )
checkEquals( ts$Avg.PLRecs.Win.Trades     , 27 )
checkEquals( ts$Avg.PLRecs.Los.Trades     , 20.375 )
checkEquals( ts$Max.PLRecs.Flat.Period    , 51 )
checkEquals( ts$Percent.Time.In.Market    , 68.090452261306538 )
checkEquals( ts$RINA.Index                , -0.90635296297990031 )

checkEquals( ts$Date.Min, as.POSIXct("2002-10-21 00:30:00 UTC", tz="UTC") )
checkEquals( ts$Date.Max, as.POSIXct("2002-11-04 23:00:00", tz="UTC") )


# scoped data ---------------------------------------------------------------- -
cat("running test on the scoped data\n")

pd <- .parseISO8601("2002-10-22::2002-10-30", tz="UTC")
dates <- paste0(pd$first.time,"::",pd$last.time)
print(dates)
ts <- tradeStatsExt("forex","GBPUSD", Dates = dates) #  NetTrPL ==  -532
# ts <- tradeStatsExt("forex","GBPUSD", Dates = "2002-10-22::2002-10-30") # NetTrPL== -528

attr(ts$Date.Min, "tzone") <- "UTC"
attr(ts$Date.Max, "tzone") <- "UTC"

str(ts)


if(1) {

    # 'data.frame':	1 obs. of  40 variables:
    checkEquals( levels(ts$Portfolio)          , "forex" )
    checkEquals( levels(ts$Symbol)             , "GBPUSD" )
    checkEquals( ts$Num.Txns                   , 15 )
    checkEquals( ts$Num.Trades                 , 7 )
    checkEquals( ts$Net.Trading.PL             , -912 ) # was -532 before TZ was set
    checkEquals( ts$Avg.Trade.PL               , -87.428571428565633 )
    checkEquals( ts$Med.Trade.PL               , -126 )
    checkEquals( ts$Largest.Winner             , 364 )
    checkEquals( ts$Largest.Loser              , -396 )
    checkEquals( ts$Gross.Profits              , 528 )
    checkEquals( ts$Gross.Losses               , -1140 )
    checkEquals( ts$Std.Dev.Trade.PL           , 263.97149918306059 )
    checkEquals( ts$Percent.Positive           , 28.571428571428569 )
    checkEquals( ts$Percent.Negative           , 71.428571428571431 )
    checkEquals( ts$Profit.Factor              , 0.46315789473686186 )
    checkEquals( ts$Avg.Win.Trade              , 264 )
    checkEquals( ts$Med.Win.Trade              , 264 )
    checkEquals( ts$Avg.Losing.Trade           , -228 )
    checkEquals( ts$Med.Losing.Trade           , -246 )
    checkEquals( ts$Avg.Daily.PL               , -122.39999999999191 )
    checkEquals( ts$Med.Daily.PL               , -126 )
    checkEquals( ts$Std.Dev.Daily.PL           , 421.5765648135594 )
    checkEquals( ts$Ann.Sharpe                 , -4.6089842866502995 )
    checkEquals( ts$Max.Drawdown               , -1424 )
    checkEquals( ts$Profit.To.Max.Draw         ,  -0.6404494382022472 ) # -0.37359550561797755
    checkEquals( ts$Avg.WinLoss.Ratio          , 1.1578947368421546 )
    checkEquals( ts$Med.WinLoss.Ratio          , 1.0731707317073498 )
    checkEquals( ts$Max.Equity                 , 400 )
    checkEquals( ts$Min.Equity                 , -1024 )
    checkEquals( ts$End.Equity                 , -912 ) # -532

    checkEquals( ts$Max.Consec.Win.Trades      , 2 )
    checkEquals( ts$Max.Consec.Los.Trades      , 4 )
    checkEquals( ts$Avg.PLRecs.All.Trades      , 24.142857142857142 )
    checkEquals( ts$Avg.PLRecs.Win.Trades      , 24 )
    checkEquals( ts$Avg.PLRecs.Los.Trades      , 24.2 )
    checkEquals( ts$Max.PLRecs.Flat.Period     , 19 )
    checkEquals( ts$Percent.Time.In.Market     , 83.928571428571431 ) #80.952380952380949
    checkEquals( ts$RINA.Index                 , -0.85190364576449917 ) # -0.91709991492573573

    checkEquals( ts$Date.Min, as.POSIXct( "2002-10-22 UTC", tz="UTC") )
    checkEquals( ts$Date.Max, as.POSIXct("2002-10-30 23:30:00 UTC", tz="UTC") )

} else {
    cat("skipping this test until time zone issue on scoped data is resolved\n")
}

cat("end of test\n")

# t(tradeStatsExt("forex","GBPUSD", Dates = "2002-10-23::2002-10-30", debugF = TRUE))
# t(tradeStatsExt("forex","GBPUSD", Dates = "2002-10-21 00:00 UST::2002-10-30 00:00 UST",debugF = TRUE))

# done!


