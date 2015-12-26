################################################################################
# Description: script that is likely to consume all memory and crash
#              to be run repeatedly until all work has been done
#
# Author: cloudcello
# Contact: IRC channel #R-Finance on "freenode" network
# Date: 2015-12-25
#
# Sources/borrowed code: luxor demos from QuantStrat modeling framework
#
################################################################################

# source("y:/_git_repository_r/testr/R_research/sma1/sma1.M3.paramset.sma.R")
###

# require(rfintools)
# require(quantstrat)
#
# setwd('y:/_git_repository_r/testr/R_research/sma1/')
#
# source("sma1.AA.include.R")
# source("sma1.AD.getSymbols.R")
# # source("sma1.M1.strategy.basic.R")()
# source("sma1.M2.add.paramsets.R")
#
# symbols=c("MSFT")
# currency='USD'
#
# ### blotter
# initPortf(portfolio.st, symbols=smb, initDate=initDate, currency=currency)
# initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency=currency)
# ### quantstrat
# initOrders(portfolio.st, initDate=initDate)


# commandArgs
# trailingOnly	-- logical. Should only arguments after --args be returned?
# If trailingOnly = TRUE, a character vector of those arguments (if any) supplied after --args.

cmdLineArgs <- commandArgs(trailingOnly=TRUE)
print(paste("cmdLineArgs:",cmdLineArgs))

# loads workspace
load(cmdLineArgs[1], verbose = TRUE)

# continue where we left off


require(doRedis)
options('redis:num'=TRUE) # prevents the nasty bug (just to make sure)

# redisHost defined "above" (in the robustApplyParamset() function's body)
registerDoRedis('jobs', host=redisHost)


# regular apply.paramset() routine:
start_t<-Sys.time()
results <- apply.paramset(strategy.st,
                          paramset.label='SMA',
                          portfolio.st=portfolio.st,
                          account.st=account.st,
                          nsamples=.nsamples,
                          verbose=TRUE,
                          user.func = backupResult,
                          user.args = list(jobDir="testFailSafe", # does not mean 'for this specific job' - for multiple jobs
                                           # should be more properly be called 'backupDir'
                                           objectName='result',
                                           jobPrefix="fubee",
                                           debugFlag=TRUE
                                           # result=result,
                                           # param.combo=param.combo
                          ),
                          verbose.wrk=FALSE,
                          packages="rfintools"
)

end_t<-Sys.time()

print("strat execution time:")
print(end_t-start_t)

stats <- results$tradeStats

# removeQueue('jobs')
# redisFlushDB()







#-------------------------------------------------------------------------------

if(0) {
    setwd('y:/_git_repository_r/testr/R_research/sma1/')
    source("sma1.M3.paramset.sma.R")

    if(0) {

        # require(rfintools)
        require(quantstrat)
        require(doRedis)
        require(rfintools)


        if(0) {


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

        } else {

        }

        # assign()

        # make a loop to assign(arg, value & perhaps pass those obj's to app.paramset)

        # save an .RData file with (host name and) process id, so the
        # process can be monitored

        if(0) {

            if(0)    print(settingsVarName)

            if(0)    importedStrategy.st= strategyToExport
            if(0)    strategy.st    = ar$strategy.st
            if(1)    paramset.label = ar$paramset.label
            if(0)    portfolio.st   = ar$portfolio.st
            if(0)    account.st     = ar$account.st
            if(0)    mktdata        = ar$mktdata
            if(0)    nsamples       = ar$nsamples
            if(1)    user.func      = ar$user.func
            if(1)    user.args      = ar$user.args
            if(1)    calc           = ar$calc
            if(0)    audit          = ar$audit
            if(1)    packages       = ar$packages
            if(0)    verbose        = ar$verbose
            if(0)    verbose.wrk    = ar$verbose.wrk
            if(1)    doRedisHost    = ar$doRedisHost

            if(0) { # now that all this is generated inside the script
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

            }



        }



        if(1) {

            #-------------------------------------------------------------------------------
            setwd('y:/_git_repository_r/testr/R_research/sma1/')

            source("sma1.AA.include.R")
            source("sma1.AD.getSymbols.R")
            # source("sma1.M1.strategy.basic.R")()
            source("sma1.M2.add.paramsets.R")
            #-------------------------------------------------------------------------------

            require(doRedis)
            options('redis:num'=TRUE) # prevents the nasty bug
            registerDoRedis('jobs', host="192.168.xxx.xxx")#doRedisHost)

            if(0) {
                # a special case is needed for this !
                registerDoSEQ() #('jobs', host="192.168.xxx.xxx")
            }


            symbols=c("MSFT")
            currency='USD'

            print(paste("running apply.paramset()..."))

            ### blotter

            # initPortf(portfolio.st, symbols='MSFT', initDate=initDate, currency='CCY')
            # initPortf(portfolio.st=ar$portfolio.st, symbols=ar$symbols, initDate=initDate, currency=currency)
            initPortf(portfolio.st, symbols, initDate=initDate, currency)
            # initAcct(account.st, portfolios=portfolio.st, initDate=initDate, currency=currency)
            initAcct(account.st, portfolios=portfolio.st, initDate, currency)

            ### quantstrat

            initOrders(portfolio.st, initDate=initDate)

            cat("about to enter apply.paramset()\n")
            # run the apply.paramset() the usual way
            results <- apply.paramset(strategy.st    = strategy.st,#ar$strategy.st,
                                      paramset.label = "SMA",#ar$paramset.label,
                                      portfolio.st   = portfolio.st,
                                      account.st     = account.st,
                                      # mktdata        = ar$mktdata,
                                      nsamples       = .nsamples,#ar$nsamples,
                                      # user.func      = ar$user.func,
                                      # user.args      = ar$user.args,
                                      # calc           = ar$calc,
                                      # audit          = ar$audit,
                                      # packages       = ar$packages,
                                      verbose        = TRUE#ar$verbose,
                                      # verbose.wrk    = ar$verbose.wrk
            )

        }
        # save output with the result into an .RData file

        cat("flushing redis\n")

        removeQueue('jobs')
        redisFlushDB()

        cat("finished!\n")


    }

}



