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

require(rfintools)

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
#-------------------------------------------------------------------------------
# ATTENTION!
# do NOT use references to internal var's of .robustR.env in main code body!
#-------------------------------------------------------------------------------
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
# script execution params:
script.testCrash  = .robustR.env$script.testCrash # crash to test stability of the main master process
# ---
# redis:
redisHost         = .robustR.env$redisHost   # IP addr. of redis server
redis.flush       = .robustR.env$redis.flush # delete queue & flush @end
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

if(._CRASHTEST || script.testCrash) {
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
# regular apply.paramset() routine:
start_t<-Sys.time()

# if 'paramsets' were NULL'able, things could be much easier
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


if(redis.flush) {
    cat("flushing redis\n")
    removeQueue('jobs')
    redisFlushDB()
    cat("done!\n")
} else {
    cat("leaving redis as is (not flushing)\n")
}

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

}

