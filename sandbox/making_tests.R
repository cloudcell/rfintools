ts$Num.Txns
ts$Num.Trades
ts$Net.Trading.PL
ts$Avg.Trade.PL
ts$Med.Trade.PL
ts$Largest.Winner
ts$Largest.Loser
ts$Gross.Profits
ts$Gross.Losses
ts$Std.Dev.Trade.PL
ts$Percent.Positive
ts$Percent.Negative
ts$Profit.Factor
ts$Avg.Win.Trade
ts$Med.Win.Trade
ts$Avg.Losing.Trade
ts$Med.Losing.Trade
ts$Avg.Daily.PL
ts$Med.Daily.PL
ts$Std.Dev.Daily.PL
ts$Ann.Sharpe
ts$Max.Drawdown
ts$Profit.To.Max.Draw
ts$Avg.WinLoss.Ratio
ts$Med.WinLoss.Ratio
ts$Max.Equity
ts$Min.Equity
ts$End.Equity
ts$Max.Consec.Win.Trades
ts$Max.Consec.Los.Trades
ts$Avg.PLRecs.All.Trades
ts$Avg.PLRecs.Win.Trades
ts$Avg.PLRecs.Los.Trades
ts$Max.PLRecs.Flat.Period
ts$Percent.Time.In.Market
ts$RINA.Index
ts$Date.Min

ts$Date.Max

checkEquals( ts$Num.Txns               ,     14                              )
checkEquals( ts$Num.Trades             ,      7                              )
checkEquals( ts$Net.Trading.PL         ,   -382                              )
checkEquals( ts$Avg.Trade.PL           ,   -107.42857142856978               )
checkEquals( ts$Med.Trade.PL           ,   -245.99999999999577               )
checkEquals( ts$Largest.Winner         ,    364.00000000000364               )
checkEquals( ts$Largest.Loser          ,   -396.00000000000148               )
checkEquals( ts$Gross.Profits          ,    528.00000000000705               )
checkEquals( ts$Gross.Losses           ,  -1279.9999999999957                )
checkEquals( ts$Std.Dev.Trade.PL       ,    272.54532170072878               )
checkEquals( ts$Percent.Positive       ,     28.571428571428569              )
checkEquals( ts$Percent.Negative       ,     71.428571428571431              )
checkEquals( ts$Profit.Factor          ,      0.41250000000000692            )
checkEquals( ts$Avg.Win.Trade          ,    264.00000000000352               )
checkEquals( ts$Med.Win.Trade          ,    264.00000000000352               )
checkEquals( ts$Avg.Losing.Trade       ,   -255.99999999999912               )
checkEquals( ts$Med.Losing.Trade       ,   -255.99999999999466               )
checkEquals( ts$Avg.Daily.PL           ,   -150.3999999999977                )
checkEquals( ts$Med.Daily.PL           ,   -255.99999999999466               )
checkEquals( ts$Std.Dev.Daily.PL       ,    426.4959554321735                )
checkEquals( ts$Ann.Sharpe             ,     -5.5980038091694011             )
checkEquals( ts$Max.Drawdown           ,   -948                              )
checkEquals( ts$Profit.To.Max.Draw     ,     -0.40295358649789031            )
checkEquals( ts$Avg.WinLoss.Ratio      ,      1.0312500000000173             )
checkEquals( ts$Med.WinLoss.Ratio      ,      1.0312500000000353             )
checkEquals( ts$Max.Equity             ,    150                              )
checkEquals( ts$Min.Equity             ,   -798                              )
checkEquals( ts$End.Equity             ,   -382                              )
checkEquals( ts$Max.Consec.Win.Trades  ,      2                              )
checkEquals( ts$Max.Consec.Los.Trades  ,      3                              )
checkEquals( ts$Avg.PLRecs.All.Trades  ,     21                              )
checkEquals( ts$Avg.PLRecs.Win.Trades  ,     24                              )
checkEquals( ts$Avg.PLRecs.Los.Trades  ,     19.800000000000001              )
checkEquals( ts$Max.PLRecs.Flat.Period ,     19                              )
checkEquals( ts$Percent.Time.In.Market ,     71.428571428571431              )
checkEquals( ts$RINA.Index             ,     -5.5689672544079754             )
checkEquals( ts$Date.Min               ,       "2002-10-23 UTC"              )
checkEquals( ts$Date.Max               ,       "2002-10-31 23:30:00 UTC"     )


has.Qty

