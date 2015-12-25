
# Description: script that is likely to consume all memory and crash
#              to be run repeatedly until all work has been done
#
# Author: cloudcello
# Date: 2015-12-25
#
#

# require(rfintools)
require(quantstrat)

# commandArgs
# trailingOnly	-- logical. Should only arguments after --args be returned?
# If trailingOnly = TRUE, a character vector of those arguments (if any) supplied after --args.

cmdLineArgs <- commandArgs(trailingOnly=TRUE)
print(cmdLineArgs)

settings <- load(cmdLineArgs[1])
print(settings)


objectsVector.char <- load(file="robustApplyParamsetParams.RData")
print(objectsVector.char)

a <- applyStrategyArgs

paramsets=NULL
if(!is.null(a$paramsets)){
    paramsets=a$paramsets
} else {
    rm(paramsets)
}

args <- substitute(list(...))[-1]
print(args)

argnames <- names(args)
print(argnames)

# save an .RData file with (host name and) process id, so the
# process can be monitored

if(1) {
    # run the apply.paramset() the usual way
    results <- apply.paramset(strategy.st    = a$strategy.st,
                              paramset.label = a$paramset.label,
                              portfolio.st   = a$portfolio.st,
                              account.st     = a$account.st,
                              mktdata        = a$mktdata,
                              nsamples       = a$nsamples,
                              user.func      = a$user.func,
                              user.args      = a$user.args,
                              calc           = a$calc,
                              audit          = a$audit,
                              packages       = a$packages,
                              verbose        = a$verbose,
                              verbose.wrk    = a$verbose.wrk
                              )

}
# save output with the result into an .RData file







