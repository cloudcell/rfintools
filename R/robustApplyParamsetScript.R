
# Description: script that is likely to consume all memory and crash
#              to be run repeatedly until all work has been done
#
# Author: cloudcello
# Date: 2015-12-25
#
#

# require(rfintools)
require(quantstrat)
require(doRedis)

# commandArgs
# trailingOnly	-- logical. Should only arguments after --args be returned?
# If trailingOnly = TRUE, a character vector of those arguments (if any) supplied after --args.

cmdLineArgs <- commandArgs(trailingOnly=TRUE)
print(paste("cmdLineArgs:",cmdLineArgs))

settingsVarName <- load(cmdLineArgs[1])
print(paste("settingsVarName:", settingsVarName, " --> printing structure:"))
settingsVar <- get(settingsVarName)
print(str(settingsVar))

# objectsVector.char <- load(file="robustApplyParamsetParams.RData")
# print(paste("objectsVector.char:", objectsVector.char))

ar <- settingsVar # applyStrategyArgs
str(ar)

rc <- try(get("ar$paramsets"), silent = TRUE)
if(!(class(rc)=="try-error")) {
    paramsets=ar$paramsets
}

# print(paste("missing:", missing(a$paramsets))
# paramsets=a$paramsets
# if(is.null(paramsets)){
#     rm(paramsets)
# }

args <- substitute(list(...))[-1]
print(args)

argnames <- names(args)
print(argnames)

# assign()

# make a loop to assign(arg, value & perhaps pass those obj's to app.paramset)

# save an .RData file with (host name and) process id, so the
# process can be monitored

if(1) {
    require(doRedis)
    options('redis:num'=TRUE) # prevents the nasty bug
    # registerDoRedis('jobs', host="192.168.xxx.xxx")
    registerDoSEQ() #('jobs', host="192.168.xxx.xxx")


print(settingsVarName)

importedStrategy.st= strategyToExport
    strategy.st    = ar$strategy.st
    paramset.label = ar$paramset.label
    portfolio.st   = ar$portfolio.st
    account.st     = ar$account.st
    mktdata        = ar$mktdata
    nsamples       = ar$nsamples
    user.func      = ar$user.func
    user.args      = ar$user.args
    calc           = ar$calc
    audit          = ar$audit
    packages       = ar$packages
    verbose        = ar$verbose
    verbose.wrk    = ar$verbose.wrk



if(1){
    print(paste("running apply.paramset()..."))

    # ".strategy"
    .strategy <- new.env()

    assign("strategyName",strategyName)
    assign(strategyName,importedStrategy.st)

    assign(x=strategyName,value=importedStrategy.st, pos=.strategy)

    ls(pos=.strategy) #, envir=".strategy")
    # environmentName(e, "test")
    # str(e)
    # class(e)
    # environmentName(e) <- "test"






    ### blotter

    initPortf(portfolio.st, symbols='MSFT', initDate=initDate, currency='CCY')
    initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency='CCY')

    ### quantstrat

    initOrders(portfolio.st, initDate=initDate)




    # run the apply.paramset() the usual way
    results <- apply.paramset(strategy.st    = ar$strategy.st,
                              paramset.label = ar$paramset.label,
                              portfolio.st   = ar$portfolio.st,
                              account.st     = ar$account.st,
                              mktdata        = ar$mktdata,
                              nsamples       = ar$nsamples,
                              user.func      = ar$user.func,
                              user.args      = ar$user.args,
                              calc           = ar$calc,
                              audit          = ar$audit,
                              packages       = ar$packages,
                              verbose        = ar$verbose,
                              verbose.wrk    = ar$verbose.wrk
                              )
}
}
# save output with the result into an .RData file







