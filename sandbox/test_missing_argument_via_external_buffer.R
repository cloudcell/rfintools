

#  Description:
#  this code is used for testing the prototype of missing-variable-state
#  transfer into an external process environment
#
#  Also, demonstrates that it is impractical to use the 'missing' state
#  feature for functions whose execution could be "exported" into
#  a separate R process as the number of alternative 'call' to
#  such functions that must be created == n^2 where 'n' is the number
#  of allowed 'missing' arguments of a function
#
#  One can 'wrap' functions with optionally missing arguments
#  if they are in the same R process only
#  as described here: http://stackoverflow.com/questions/28875389/r-how-to-pass-a-potentially-missing-argument?rq=1

robustRenv.must.exist <- function(env=globalenv())
{
    if(exists(x = ".robustR.env", envir = env)) {
        if(!is.environment(env$.robustR.env)) {
            stop("Conflict: .robustR.env is present but is not an environment")
        }
    } else {
        cat("creating the environment \".robustR.env\" ... ")
        env$.robustR.env <- new.env()
        cat(" done\n")
    }
}

# this function is run locally (say, in the main RStudio environment)
apply.paramset.wrapper <- function(somevar=TRUE, paramsets, ...) {
    robustRenv.must.exist(globalenv())
    # quantstrat:::must.have.args(match.call(), c('somevar'))#, 'paramset.label', 'portfolio.st'))
    if(missing(paramsets)) {
        cat("missing arg. \"paramsets\"\n")
        .robustR.env$applPara.paramsets.missing = TRUE
        if(exists("applPara.paramsets", envir = globalenv()$.robustR.env)) {
            rm("applPara.paramsets", envir = globalenv()$.robustR.env)
        }
    } else {
        cat("present arg. \"paramsets\"\n")
        .robustR.env$applPara.paramsets.missing = FALSE
        .robustR.env$applPara.paramsets = paramsets
    }

    # "assertion" (just in case)
    if(is.na(.robustR.env$applPara.paramsets.missing)||
       is.null(.robustR.env$applPara.paramsets.missing))
        stop(paste("Something went wrong! NA/NULL not allowed",
                   "in \".robustR.env$applPara.paramsets.missing\"!"))

    if(1) {
        # deal with "missingness" in this "bloated" way:
        if(.robustR.env$applPara.paramsets.missing) {
            # run apply.paramset without the argument "paramsets"
            apply.paramset.test.missing( x=6, ...)
        } else {
            # run apply.paramset with the argument "paramsets"
            apply.paramset.test.missing(
                x=5, paramsets.low = .robustR.env$applPara.paramsets, ...)
        }
    } else {
        # here, the code must save the environment
        save.image("workspace.RData")

        # and run the script
        # run script which will save its result / output in a .RData file
        # (to be read after script has finished working)
        system2(command="Rscript",
                args=c(scriptFileFullPath,
                       workspaceFileFullPath,
                       scriptOutputFileFullPath), # pass thru cmdLine args
                wait = TRUE
        )#, scriptSetupFile, scriptOutputFile))
    }
}



#-------------------------------------------------------------------------------
# the two functions below are to be run in a separate process
# (move them out into a separate script, and load the full R workspace saved
# after running the function apply.paramsets.wrapper)
# the code for saving/loading the workspace is not shown here
#-------------------------------------------------------------------------------

# all the data has been passed via ".robustR.env" environment

# the receptac
apply.paramset.wrapper.externalPart <- function()
{
    # deal with "missingness" in this "bloated" way:
    if(.robustR.env$applPara.paramsets.missing) {
        # run apply.paramset without the argument "paramsets"
        apply.paramset.test.missing( x=6, ...)
    } else {
        # run apply.paramset with the argument "paramsets"
        apply.paramset.test.missing(
            x=5, paramsets.low = .robustR.env$applPara.paramsets, ...)
    }
}

apply.paramset.test.missing <- function(paramsets.low, ...)
{
    if(missing(paramsets.low)){
        cat("missing parameter \"paramsets.low\"\n")
        try(cat("paramsets.low = ",paramsets.low))
        cat("^^^ if you see an error reported above --> this f() works correctly")
    } else {
        cat("present parameter \"paramsets.low\"\n")
        cat("paramsets.low = ",paramsets.low)
    }
}


#-------------------------------------------------------------------------------
# test code
if(0) {
    rm(.robustR.env, envir = globalenv())
    robustRenv.must.exist()

    load("workspace.RData",verbose = TRUE)
    apply.paramset.wrapper.externalPart()

    # see what happens when it's missing
    apply.paramset.wrapper(somevar=1)
    .robustR.env$applPara.paramsets.missing
    .robustR.env$applPara.paramsets
    str(as.list(ls(envir = .robustR.env)))
    paramsets = .robustR.env$applPara.paramsets
    paramsets

    # see what happens when it's NOT missing
    apply.paramset.wrapper(somevar=1, paramsets = "kpp")
    .robustR.env$applPara.paramsets.missing
    .robustR.env$applPara.paramsets
    str(as.list(ls(envir = .robustR.env)))
    paramsets = .robustR.env$applPara.paramsets

    ls_environments()
}

#-------------------------------------------------------------------------------
# local 'sandbox'
if (0) {

#  Source: http://stackoverflow.com/questions/28875389/r-how-to-pass-a-potentially-missing-argument?rq=1)

    ## low level:
    foo_low <- function(x, argA, argB){
        if(!missing(argA)) warning("argA was given")
        if(!missing(argB)) warning("argB was given")
        x+1
    }
    ## wrapper/high level:
    foo_high <- function(x, argA, argB){
        if(!missing(argA)) {
            .robustR.env$applPara.paramsets=argA
        } else {
            if(exists(x = "applPara.paramsets",envir = .robustR.env)) {
                rm("applPara.paramsets",envir = .robustR.env)}
        }
        foo_low(x=x, argA=.robustR.env$applPara.paramsets, argB=argB)
    }

    foo_low(1)

    foo_high(x=2)


    foo_low(x=5, {if(1){assign("argB",1,envir = parent.frame())}else{assign("argA",1,envir = parent.frame())}})

    foo_low(x=5)

}


