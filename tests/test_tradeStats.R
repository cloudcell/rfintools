

require(RUnit)
require(quantstrat)
require(rfintools)


dir=system.file('data',package='quantstrat')
load(file = paste0(dir,"/","test_tradeStats.RData"))

put.portfolio(portf2,portfolio.st = "forex")

getPortfolio("forex")$symbols$GBPUSD$txn
getPortfolio("forex")$symbols$GBPUSD$posPL

# full data --------------------------------------------------------------------
ts <- tradeStats("forex","GBPUSD")

str(ts)
# 'data.frame':	1 obs. of  40 variables:
#     $ Portfolio                 : Factor w/ 1 level "forex": 1
# $ Symbol                    : Factor w/ 1 level "GBPUSD": 1
# $ Num.Txns                  : num 24
# $ Num.Trades                : int 12
# $ Net.Trading.PL            : num -492
# $ Avg.Trade.PL              : num -41
# $ Med.Trade.PL              : num -136
# $ Largest.Winner            : num 414
# $ Largest.Loser             : num -396
# $ Gross.Profits             : num 1296
# $ Gross.Losses              : num -1788
# $ Std.Dev.Trade.PL          : num 285
# $ Percent.Positive          : num 33.3
# $ Percent.Negative          : num 66.7
# $ Profit.Factor             : num 0.725
# $ Avg.Win.Trade             : num 324
# $ Med.Win.Trade             : num 359
# $ Avg.Losing.Trade          : num -223
# $ Med.Losing.Trade          : num -241
# $ Avg.Daily.PL              : num -61.5
# $ Med.Daily.PL              : num -121
# $ Std.Dev.Daily.PL          : num 378
# $ Ann.Sharpe                : num -2.58
# $ Max.Drawdown              : num -1424
# $ Profit.To.Max.Draw        : num -0.346
# $ Avg.WinLoss.Ratio         : num 1.45
# $ Med.WinLoss.Ratio         : num 1.49
# $ Max.Equity                : num 400
# $ Min.Equity                : num -1024
# $ End.Equity                : num -492
# $ Max.Consec.Winning.Trades : int 2
# $ Max.Consec.Losing.Trades  : int 4
# $ Avg.Bars.In.Total.Trades  : num 22.6
# $ Avg.Bars.In.Winning.Trades: num 27
# $ Avg.Bars.In.Losing.Trades : num 20.4
# $ Max.Bars.Flat.Period      : int 51
# $ Percent.Time.In.Market    : num 68.1
# $ RINA.Index                : num -0.906
# $ Date.Min                  : POSIXct, format: "2002-10-21 04:30:00"
# $ Date.Max                  : POSIXct, format: "2002-11-05 02:00:00"

# full data --------------------------------------------------------------------





getPortfolio("forex")$symbols$GBPUSD$txn["2002-10-21::2002-10-31"]

ts2 <- tradeStats("forex","GBPUSD", Dates = "2002-10-22::2002-10-31")

str(ts2)

str(ts2)
# 'data.frame':	1 obs. of  40 variables:
#     $ Portfolio                 : Factor w/ 1 level "forex": 1
# $ Symbol                    : Factor w/ 1 level "GBPUSD": 1
# $ Num.Txns                  : num 16
# $ Num.Trades                : int 8
# $ Net.Trading.PL            : num -478
# $ Avg.Trade.PL              : num -110
# $ Med.Trade.PL              : num -186
# $ Largest.Winner            : num 364
# $ Largest.Loser             : num -396
# $ Gross.Profits             : num 528
# $ Gross.Losses              : num -1406
# $ Std.Dev.Trade.PL          : num 252
# $ Percent.Positive          : num 25
# $ Percent.Negative          : num 75
# $ Profit.Factor             : num 0.376
# $ Avg.Win.Trade             : num 264
# $ Med.Win.Trade             : num 264
# $ Avg.Losing.Trade          : num -234
# $ Med.Losing.Trade          : num -251
# $ Avg.Daily.PL              : num -146
# $ Med.Daily.PL              : num -191
# $ Std.Dev.Daily.PL          : num 382
# $ Ann.Sharpe                : num -6.09
# $ Max.Drawdown              : num -1424
# $ Profit.To.Max.Draw        : num -0.336
# $ Avg.WinLoss.Ratio         : num 1.13
# $ Med.WinLoss.Ratio         : num 1.05
# $ Max.Equity                : num 400
# $ Min.Equity                : num -1024
# $ End.Equity                : num -478
# $ Max.Consec.Winning.Trades : int 2
# $ Max.Consec.Losing.Trades  : int 4
# $ Avg.Bars.In.Total.Trades  : num 23.9
# $ Avg.Bars.In.Winning.Trades: num 24
# $ Avg.Bars.In.Losing.Trades : num 23.8
# $ Max.Bars.Flat.Period      : int 19
# $ Percent.Time.In.Market    : num 78.8
# $ RINA.Index                : num -1.29
# $ Date.Min                  : POSIXct, format: "2002-10-22"
# $ Date.Max                  : POSIXct, format: "2002-10-31 23:30:00"
















t(tradeStats("forex","GBPUSD", Dates = "2002-10-23::2002-10-30", debug = TRUE))
t(tradeStats("forex","GBPUSD", Dates = "2002-10-21 00:00 UST::2002-10-30 00:00 UST",debug = TRUE))






