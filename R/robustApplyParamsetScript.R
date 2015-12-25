
# Description: script that is likely to consume all memory and crash
#              to be run repeatedly until all work has been done
#
# Author: cloudcello
# Date: 2015-12-25

require(rfintools)
require(quantstrat)

objectsVector.char <- load(file="robustApplyParamsetParams.RData")

a <- applyStrategyArgs

if(!missing(a$paramsets)){
    paramsets=a$paramsets
}

args <- substitute(list(...))[-1]
argnames <- names(args)


# save an .RData file with (host name and) process id, so the
# process can be monitored

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
                          verbose.wrk    = a$verbose.wrk,
                          ...)

# save output with the result into an .RData file

