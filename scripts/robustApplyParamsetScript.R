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

# commandArgs
# trailingOnly	-- logical. Should only arguments after --args be returned?
# If trailingOnly = TRUE, a character vector of those arguments (if any) supplied after --args.
cmdLineArgs <- commandArgs(trailingOnly=TRUE)
print(paste("cmdLineArgs:",cmdLineArgs))

# loads workspace
load(cmdLineArgs[1], verbose = TRUE)

# file to transfer results to
scriptOutputFileFullPath=cmdLineArgs[2]

#===============================================================================
#--INTERNAL--BOILERPLATE-CODE---------------------------------------------------
# this environment is used as a channel of communication for ensuring
# smooth and carefree "user experience" ;)
checkRobustR.env()
#-------------------------------------------------------------------------------
# --in-->[_]
# < ...empty... >
#-------------------------------------------------------------------------------
# [_]--out->
# backup function:
backup.func       = .robustR.env$backup.func       # function to save backups
backup.jobDir     = .robustR.env$backup.jobDir     # netw. path in redisWorker.conf
backup.jobPrefix  = .robustR.env$backup.jobPrefix  # prefix to find all completed runs
backup.objectName = .robustR.env$backup.objectName # can be used within ANY function
backup.debugFlag  = .robustR.env$backup.debugFlag  # separate file with extra debug info
# ---
# redis:
redisHost         = .robustR.env$redisHost   # IP addr. of redis server
# ---
# sent via command line: (inactive here)
# tmp.dir = .robustR.env$script.commDir    # comm.chnl "robustR <--> fragileR"
# tmp.file =.robustR.env$script.commFile   # script communic'n file name
# ---
# apply.paramset():
strategy.st       = .robustR.env$applPara.strategy.st
paramset.label    = .robustR.env$applPara.paramset.label
portfolio.st      = .robustR.env$applPara.portfolio.st
account.st        = .robustR.env$applPara.account.st
mktdata           = .robustR.env$applPara.mktdata
nsamples          = .robustR.env$applPara.nsamples
user.func         = .robustR.env$applPara.user.func # not used at the moment
user.args         = .robustR.env$applPara.user.args # not used at the moment
calc              = .robustR.env$applPara.calc
audit             = .robustR.env$applPara.audit
packages          = .robustR.env$applPara.packages
verbose           = .robustR.env$applPara.verbose
verbose.wrk       = .robustR.env$applPara.verbose.wrk
paramsets         = .robustR.env$applPara.paramsets
meta.missing.ps   = .robustR.env$applPara.paramsets.missing
if(class(.robustR.env$applPara.ellipsis)=="list") stop("unprocessed args!") # FIXME!
#-------------------------------------------------------------------------------
#===============================================================================


################################################################################
################################################################################
# continue where we left off

# do we just want to crash ? ;)
._CRASHTEST=FALSE

if(._CRASHTEST) {
    # eat <- function() { for(i in seq(1000)) assign(paste0("var",i),vector(length=i^5)) }
    # eat()
    if(0){
        # Source: http://stackoverflow.com/questions/25139247/how-to-crash-r
        require(devtools)
        install_github("jdanielnd/crash") # FIXME: must run if required only
        require(crash)
        crash()
    }
    # Source: Dirk Eddelbuettel http://stackoverflow.com/questions/25139247/how-to-crash-r
    library(inline)
    crashMe <- cfunction(body="::abort();")
    crashMe()
}

require(doRedis)
options('redis:num'=TRUE) # prevents the nasty bug (just to make sure)

# redisHost defined "above" (in the robustApplyParamset() function's body)
registerDoRedis('jobs', host=redisHost)


require(quantstrat)
require(rfintools)
# regular apply.paramset() routine:
start_t<-Sys.time()

if(meta.missing.ps) {
    results <- apply.paramset(
        strategy.st    = strategy.st,
        paramset.label = paramset.label,
        portfolio.st   = portfolio.st,
        account.st     = account.st,
        nsamples       = nsamples,
        verbose        = verbose,
        user.func      = backup.func,
        user.args      = list(
            jobDir     = backup.jobDir,
            jobPrefix  = backup.jobPrefix,
            objectName = backup.objectName,
            debugFlag  = backup.debugFlag
        ),
        verbose.wrk    = verbose.wrk,
        packages       = c("rfintools", packages)
        # paramsets      = paramsets
    )
} else {
    results <- apply.paramset(
        strategy.st    = strategy.st,
        paramset.label = paramset.label,
        portfolio.st   = portfolio.st,
        account.st     = account.st,
        nsamples       = nsamples,
        verbose        = verbose,
        user.func      = backup.func,
        user.args      = list(
            jobDir     = backup.jobDir,
            jobPrefix  = backup.jobPrefix,
            objectName = backup.objectName,
            debugFlag  = backup.debugFlag
        ),
        verbose.wrk    = verbose.wrk,
        packages       = c("rfintools", packages),
        paramsets      = paramsets
    )

}


end_t<-Sys.time()

print("strat execution time:")
print(end_t-start_t)

# stats <- results$tradeStats
save("results", file=scriptOutputFileFullPath)
print( paste("\'results\' saved in",scriptOutputFileFullPath) )


cat("flushing redis\n")
removeQueue('jobs')
redisFlushDB()
cat("done!\n")

# FIXME: write a function that will wipe the used arguments in this
# "communication channel" properly
# .robustR.env$applPara.ellipsis <- list()
.robustR.env$applPara.ellipsis <- NULL



cat("End of script\n")
cat("Bye!\n")

# removeQueue('jobs')
# redisFlushDB()

# and we're done!
################################################################################





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

        cat("done!\n")


    }





