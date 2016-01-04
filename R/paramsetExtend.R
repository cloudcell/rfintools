

# Author: cloudcello (except where noted otherwise)
# Date: 2016-01-04
#
# Description: adds functionality to paramset tweaking
#    function: paramset.extend()
#              use case: initial paramset needs to be extended in such a
#                        way so as to preserve combo numbers, so as to
#                        correspond to file names of backups
#



# appends new combos from 'ext.ps' to 'main.ps'
# while preserving row numbers in 'main.ps'
paramset.extend <- function(main.ps=NULL, ext.ps=NULL)
{
    require(plyr)
    # Help on "join()":
    # The four join types return:
    # inner: only rows with matching keys in both x and y
    # left: all rows in x, adding matching columns from y
    # right: all rows in y, adding matching columns from x
    # full: all rows in x with matching columns in y,
    #       then the rows of y that don't match x.
    #
    # "Unlike merge, preserves the order of x no matter what join type is used.
    # If needed, rows from y will be added to the bottom."

    join(main.ps, ext.ps, type='full')

    # Alternative solution:
    # http://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
    # creates an id field, joins two df's (sorting by the id field)
}




# Source: R package 'quantstrat' (factored out)
# || transferred from the 'forked' copy of QS in my github repo  to make
# || this code more compatible with the official QS version
# XXX This function duplicates what the code in apply.paramset does
# however, if paramsets can come from some external source (as the
# function apply.paramset implies, there might be a reason for such
# function to exist
# ( I personally need this to be able to restart crashed apply.paramsets() )
# Since functions expand.distributions() and apply.constraints() are
# not officially exported, it makes sense either to do so (to export them)
# or to add this utility function here and export _it_ (need feedback on this)
# generate.paramset  (deprecated)
paramset.generate <- function( strategy.st, paramset.label, nsamples=0 )
{

    strategy <- quantstrat:::must.be.strategy(strategy.st)
    quantstrat:::must.be.paramset(strategy, paramset.label)

    distributions <- strategy$paramsets[[paramset.label]]$distributions
    constraints <- strategy$paramsets[[paramset.label]]$constraints

    param.combos <- quantstrat:::expand.distributions(distributions)
    param.combos <- quantstrat:::apply.constraints(constraints, distributions, param.combos)
    rownames(param.combos) <- NULL  # reset rownames

    # A Use Case for This Feature:
    # 1. An analyst may initially want to sample a small area and save both
    #    the paramsets and calculations.
    # 2. After preliminary analysis, an analyst may want to increase the sample size.
    #    Generation of a sample outside the function apply.paramset() allows to
    #    reduce calculations by removing previously calculated combinations of
    #    parameters in a separate step.
    if(nsamples > 0)
        param.combos <- quantstrat:::select.samples(nsamples, param.combos)

    paramsets <- param.combos
    paramsets
}

# <sandbox area> ---------------------------------------------------------------
if(0) {
    # paramset_main <- generate.paramset(strategy.st=strategy.st,paramset.label='SMA')
    paramset_main_ext <- generate.paramset(strategy.st=strategy.st,paramset.label='SMA')
    # length()
    dimNum <- dim(paramset_main)[2]
    recNum <- nrow(paramset_main)
    merge
    for(i in seq(dimNum)) {

    }


}

