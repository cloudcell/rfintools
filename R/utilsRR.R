


# returns a reference to robustR environment if it is present / or was created,
# fails otherwise
robustRenv.must.exist <- function(env=globalenv())
{
    if(exists(x = ".robustR.env", envir = env)) {
        if(!is.environment(env$.robustR.env)) {
            stop("Conflict: object \'.robustR.env\' is present but is not an environment")
        }
    } else {
        cat("creating the environment \".robustR.env\" ... ")
        # env$.robustR.env <- new.env()
        env$.robustR.env <- new.env(hash = TRUE, parent = globalenv())
        cat(" done\n")
    }
    get(x = ".robustR.env",envir = env)
}

# returns a reference to robustR environment if it is present, fails otherwise
checkRobustR.env <- function(env=globalenv())
{
    if(!exists(".robustR.env", envir = env)) stop(".robustR.env missing!")
    get(x = ".robustR.env",envir = env)
}

# Function description:
# robustRSetup is used to create the main 'communication channel' across all
# the components of the framework.
# The framework can be adjusted for any function that uses parallel processing
# (with minimum adjustments to those functions). Those functions must be able
# to accept 'user.functions' and could save intermediary state of its internals
# The default object that is saved is named "result" (see defaults below).
robustRSetup <- function( backup.func       = backupResult,
                          backup.cfgFile    = "redisWorker.conf",
                          backup.jobDir     = "testFailSafe",
                          backup.jobPrefix  = "fubee",
                          backup.objectName = "result",
                          backup.debugFlag  = TRUE,
                          redisHost         = "127.0.0.1", #"192.168.x.x",
                          redis.flush       = FALSE,
                          script.commDir    = "c:/R/work",
                          script.commFile   = "scrComm.RData",
                          script.testCrash  = FALSE)
{
    #===============================================================================
    #--USER-SET--BOILERPLATE-CODE---(TO-BE-HIDDEN-FROM-VIEW-SOON)-------------------
    #---------------------------------------------------------------------------
    # ATTENTION!
    # do NOT use references to internal var's of .robustR.env in main code body!
    #---------------------------------------------------------------------------
    # this environment is used as a channel of communication for ensuring
    # smooth and carefree "user experience" ;)
    robustRenv.must.exist() # creates the environment if not present
    #-------------------------------------------------------------------------------
    # --in-->[_]
    .robustR.env$backup.func       = backup.func    # function to save backups
    .robustR.env$backup.cfgFile    = backup.cfgFile # path for each worker
    .robustR.env$backup.jobDir     = backup.jobDir  # dir appended to path
    .robustR.env$backup.jobPrefix  = backup.jobPrefix   # prefix to find all completed runs
    .robustR.env$backup.objectName = backup.objectName  # can be used within ANY function
    .robustR.env$backup.debugFlag  = backup.debugFlag   # separate file with extra debug info
    .robustR.env$redisHost         = redisHost       # IP addr. of redis server
    .robustR.env$redis.flush       = redis.flush   # delete queue & flush @end
    .robustR.env$script.commDir    = script.commDir  # comm.chnl "robustR <--> fragileR"
    .robustR.env$script.commFile   = script.commFile # script communic'n file name
    .robustR.env$script.testCrash  = script.testCrash # crash to test stability of the main master process
    #-------------------------------------------------------------------------------
    # [_]--out->
    # < ...empty... >
    #-------------------------------------------------------------------------------
    #===============================================================================
}

# creates a new robustR environment and returns a reference to it
robustRReset.env <- function(env=globalenv())
{
    if(exists(x = ".robustR.env", envir = env)) {
        if(!is.environment(env$.robustR.env)) {
            stop("Conflict: object \'.robustR.env\' is present but is not an environment")
        }
        env$.robustR.env <- new.env(hash = TRUE, parent = globalenv())
    } else {
        cat("creating the environment \'.robustR.env\' ... ")
        # env$.robustR.env <- new.env()
        env$.robustR.env <- new.env(hash = TRUE, parent = globalenv())
        cat(" done\n")
    }
    gc()
    get(x = ".robustR.env",envir = env)
}

