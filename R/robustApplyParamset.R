
## < section: description > ####################################################
# Description:
#    Saves data calculated on worker machines to a destination specified
#    in the config file that must be located in the same folder a worker
#    script is run
#
#    Solves the issue of network location mapping in different OS's:
#      Windows machines may use the format "//host1/shared_location"
#      Linux machines may use either local "/home/user/shared_location"
#                                          (or any other mapped location)
#
#    The usual modus operandi: run - fail - retrieve all saved data -
#    reconstruct the list of missing data - create new paramset in
#    a temporary strategy object - run again until completion
#
# Author: cloudcello
# Date: 2015-12-23
#
# Acknowledgements:
# Some code borrowed from the following sources:
#  * QuantStrat modeling framework ( in generate.paramsets() )

############################################################################## #
# References:
#
# finding a path of a souced file:
#    http://stackoverflow.com/questions/3452086/getting-path-of-an-r-script
#    Dirk argues that creating a package is the way to go
#
# creating folders with shared read/write permissions:
#    http://superuser.com/questions/280994/give-write-permissions-to-multiple-users-on-a-folder-in-ubuntu
#
# referincing a local environment:
#    http://stackoverflow.com/questions/14399205/in-r-how-to-make-the-variables-inside-a-function-available-to-the-lower-level-f
#    http://stackoverflow.com/questions/8771942/how-can-i-reference-the-local-environment-within-a-function-in-r
#    help(get)
#    http://blog.obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/
#
# running R scripts:
#    http://stackoverflow.com/questions/4808169/r-command-line-passing-a-filename-to-script-in-arguments-windows
#
# creating an R package properly:
#    http://r-pkgs.had.co.nz/misc.html (standard folders)
#


############################################################################## #
# Infrastructure:
# 0. A master process creates a destination folder (jobDir) within a
#    prespecified remoteBaseDirectory ( remoteBaseDir ).
#    Workers will not access the destination until they have been given a job
#    to process. If workers are allowed to create folders, they might
#    compete against one another in creating a not yet existing "jobDir"
#    and might fail, crashing the foreach process (it may fail at combine
#    stage if one worker produces an error).
# 1. The master process supplies each worker with "jobDir" to be appended
#    to path of 'remoteBaseDir'
# 1. A worker reads a 'map' file supplied with each machine.
# 2. If a worker finds its own host (machine) name, it checks whether
#    destination (remoteBaseDir/jobDir) exists. Only if the destination
#    for writing data is found by a worker, does the process continue;
#    otherwise the worker shuts itself down (crashes).
# 3. A worker uses the destination path (remoteBaseDir/jobDir) for writing
#    backup data.
#

############################################################################## #
# Configuration File Format:
#    Config. files must have the following format ("<-space(s)->" == "\s+")
#    ## HOSTNAME <-space(s)-> ANY_REACHEABLE_PATH_TO_SHARED_DIRECTORY
#    Host names must always be written in the capital case
#    Tildas in path are not allowed (no tilda expansion is performed)
#
#    Alternatively, paths could be based on an operating system, e.g.:
#    ## OS_WINDOWS //linuxhost/shared_backup_folder
#    ## OS_LINUX   /path/to/shared_backup_folder
#    (this latter alternative has not been implemented, but it's quite easy)
#
#    Example: see /data/ folder of this package, file "redisWorker.conf"

# ATTN!
#    "param.combo" and "result" are quantstrat variables
#    used within apply.paramset() function

# TODO: make a utility function for printig debug data as follows: ----
#       {function name}(): {message}
# TODO: write descriptions properly to produce help/doc'n automatically ----
#

############################################################################## #
# Function description:
#    A user function for using with apply.paramset()
#    to save backups of processed tasks to a specified location
# cfgFile The config file must be located in the same folder as the worker
#   script. Currently R has no function to determine the location of the source
#   unless the file is run through Rscript A good alternative is to create a
#   package (as Dirk suggested at StackOverflow) and get a relative location
#   based on the location of the package.
# jobDir Directory within the shared location (defined in cfgFile). The purpose
#   of jobDir is to arbitrarily seggregate backup files The name of the folder
#   does not mean a folder 'for this specific job' - for multiple jobs should be
#   more properly be called 'backupDir' This dir. must be created manually with
#   group permissions common to both external and internal users
# jobPrefix in case files from multiple jobs are saved into the same folder
# objectName a name of an object to be saved
# debugFlag to save additional debug info in a separate file, default FALSE
#------------------------------------------------------------------------------|
# Function description -- see the full description above
backupResult <- function(cfgFile="redisWorker.conf",
                         jobDir="testFailSafe", # XXX: might be renamed to 'backupDir'
                         jobPrefix="foo",
                         objectName="result",
                         debugFlag=FALSE,
                         verbose=TRUE
                         )
{
    cat("backupResult(): function entry\n")
    cat(
        paste0("backupResult(): host:",Sys.info()["nodename"],
               "-pid:",Sys.getpid(),
               " workdir: ", getwd(),
               "\n")
    )

    # get the object from the environment of the _calling_ function
    # i.e. using the 'call chain' (parent.frame()) (and NOT the parent.env() !)
    env = parent.frame()

    ###########################################################################|
    # TODO: this must be done only once (perhaps even at the level of the ----
    #      script rather than this function!!! set some variable in the
    #      worker's global(?) environment to keep any needed info from the
    #      config until the next 'job' is sent from the master process
    ###########################################################################|
    cat("backupResult(): looking for a cfg file\n")
    if(!file.exists(cfgFile)) {
        cat("backupResult(): worker config file not found in the working directory\n")
        stop( "\t Worker config file not found in the working directory" )
    } else {
        cat("backupResult(): found cfg file\n")
    }

    cfgText <- readLines(cfgFile)
    cat("backupResult(): read cfg file\n")

    # regexp:
    # {n,} The preceding item is matched n or more times.
    # + The preceding item will be matched one or more times.

    parsed1 <- strsplit(cfgText, split=' +' )
    parsed2 <- list()
    for(i in seq(length(parsed1))) {
        parsed2[parsed1[[i]][1]] <- parsed1[[i]][2]
    }

    # assume config node names are all in upper case
    thisHostName <- toupper(Sys.info()["nodename"])

    backupPathBase <- unlist(parsed2[thisHostName])

    # Note: linux shared "backupPath" by itself is not considered a directory
    # (at least by Windows)
    backupPathFull <- paste0(backupPathBase,"/", jobDir)

    if( !dir.exists(backupPathFull) ) {
        cat( paste0("backupResult(): Worker cannot access the specified backup directory:\n",
                    backupPathFull,"\n",
                  "\t Make sure it has been created by the master process") )
        quit(save="no", runLast = FALSE)
    }

    # save(backupPath,file=paste0(backupPath,"/","testBBOX.RData"))
    cat("backupResult(): full backup path has been set\n")
    ###########################################################################|
    # Now the worker has a path set
    ###########################################################################|

    cat("backupResult(): preparing a unique file name\n")

    # check whether the file to be written already exists. If so, save the file
    # with a unique suffix via "DUP" & tempfile()

    # comboName="1"
    comboName=row.names(env$param.combo) # in the env't of a _calling_ function!

    prefixedComboName <- paste0(jobPrefix, "-", comboName)

    cat(paste0("backupResult(): prefixedComboName = ", prefixedComboName, "\n"))

    baseFileName = paste0(prefixedComboName,".RData")

    cat(paste0("backupResult(): baseFileName = \'", baseFileName, "\'\n"))

    # cat(paste0("backupResult(): Backup data file [combo.number].RData = ", baseFileName, "\n"))

    fullPathAndFileName = paste0(backupPathFull, "/", baseFileName)

    # cat(paste0("backupResult(): Checking whether baseFileName = \'", baseFileName, "\' already exists: "))

    # assign a "duplicate" file name for this worker output
    if(file.exists(fullPathAndFileName)) {
        # cat("Yep!\n")
        cat(paste0("backupResult(): a file named \'", baseFileName, "\' already exists\n"))
        fullPathAndFileName <- tempfile(pattern=paste0(prefixedComboName,"_DUP_"),
                                        tmpdir=backupPathFull,
                                        fileext=".RData")

        baseFileName <- substr(fullPathAndFileName,
                               start = nchar(backupPathFull)+1L,
                               stop = nchar(fullPathAndFileName))

        # Workers running on Windows will have tempfile() produce a path with a
        # backslash (as "\\") so 1 extra character must be dealt with
        # separately:
        if(substr(baseFileName,1,1)=="\\") {
            baseFileName <- substr(baseFileName,2,nchar(baseFileName))
        }

        cat(paste0("backupResult(): assigned a new unique file name \'", baseFileName, "\'\n"))
    } else {
        cat(paste0("backupResult(): assigned a unique file name \'", baseFileName, "\'\n"))
        # cat("Nope!\n") # no file with the same name
    }

    # save(list=objectName, file="//server/data_01/aa_cluster_backups/dummy_var.RData")
    # dummyobj <- "dummy"
    # objectName <- "dummyobj"
    cat("backupResult(): saving backup...\n")
    cat(" * object to be saved:",objectName,"\n" )
    cat(" * backup location:",fullPathAndFileName,"\n" )

    # get the object from the environment of the _calling_ function
    # i.e. using the 'call chain' (parent.frame())
    # NOT the 'environment chain' (parent.env()) !
    # bakObj <- list()
    # bakObj[objectName] <- get(envir=env, objectName)#, mode="list")
    bakObj <- get(envir=env, objectName)#, mode="list")
    cat("backupResult(): got backup object", objectName ,"\n")

    # assume object to be saved is a list !
    # if(!is.list(bakObj[objectName])) {stop("object to be backed up must be a list!")}
    # if(0){
    if(!is.list(bakObj)) {
        cat("object to be backed up must be a list!\n")
        flush.console()
        stop("object to be backed up must be a list!")
    }
    # }

    # add the original name of the object to metadata obj.
    bakObjMeta <- list()
    bakObjMeta["originalBakObjName"] <- objectName
    bakObjMeta["paramsetLabelUsed"] <- get(envir=env,"paramset.label")

    # use the 'strategy' variable within the foreach 'expression'
    # i.e. strategy.st supplied by the apply.paramsets() is not available
    # within the worker's environment
    strategy.st <- get(envir=env,"strategy") # of strategy type already !

    # if(0){
    # strategy.st <- quantstrat::getStrategy(strategy)
    # strategy.st <- getStrategy(strategy)
    bakObjMeta["strategyName"] <- strategy.st$name
    # }

    cat("backupResult(): about to save bakObj and bakObjMeta\n")
    rc <- try(
        save(list=c("bakObj","bakObjMeta"), file=fullPathAndFileName ),
        silent=TRUE
    )

    if(is.null(rc)) {
        cat("backupResult(): backup saved on ", date(),"\n")
        if(verbose) { print(bakObj$tradeStats) }
        print(bakObjMeta)
    } else {
        print(rc)
    }

    if(debugFlag) {
        # debugData might be used to collect all the output from the run

        debugData <- list(sysInfo=Sys.info(), pID=Sys.getpid(),
                          comboName=comboName, jobPrefix=jobPrefix)

        debugTag <- paste("_DEBUG",
                          debugData$sysInfo["nodename"],
                          debugData$pID,
                          sep = "_")

        rc <- try(
            save(list="debugData",
                 file=paste0(fullPathAndFileName,debugTag,".RData") ),
            silent=TRUE
        )

        if(is.null(rc)) {
            cat("backupResult(): debug data saved on ", date(),"\n")
        } else {
            print(rc)
        }
    }

    cat("backupResult(): success!\n")
    cat("backupResult(): function exit\n")
    flush.console() # just to make sure
    return(0)
}


# Function Description:
# reads backup files into memory & retrieves combo numbers into a list
getProcessedCombos <- function( backupPath="//host/shared/jobDir",
                                jobPrefix=stop("jobPrefix must be specified!"),
                                verbose=FALSE,
                                returnData=FALSE,
                                saveMemory=FALSE)
{
    # path=backupPath
    # jobPrefix="fub1"
    # path="//host/shared/testFailSafe"
    pattern=paste0(jobPrefix,"-","*.RData")
    rxPattern=glob2rx(pattern)
    allJobFiles <- list.files(path=backupPath, pattern=rxPattern)

    # throw out 'DEBUG' files
    rxPattern=paste0("^.*DEBUG.*$")
    whichDebug <- grep(allJobFiles,pattern = rxPattern)
    comboJobFiles <- allJobFiles[-whichDebug]

    for(i in comboJobFiles) {
        if(verbose) print(i)
    }
    # list.files()


    # Get a list of combos that have been processed
    # The use of a list might be wasteful, but much safer as some files
    # might be unreadable and the estimate of the length of the vector
    # may turn out to be wrong
    processedCombos <- list(numbers=NULL,data=list())
    objNum=1
    for(i in comboJobFiles) {

        cat(paste0("processing file #", objNum,":"), i,"...")
        # jobPrefix="fub1"
        # i=comboJobFiles[3]
        # verbose=TRUE

        objNames <- load(paste0(backupPath,"/",i),
                         envir = environment(),
                         verbose=verbose)

        #assert length == 1
        if(length(objNames)!=2) stop("backup has a wrong number of objects")

        bakObj <- get(x="bakObj")
        bakObjMeta <- get(x="bakObjMeta")
        originalBakObjName <- bakObjMeta$originalBakObjName

        # object['result']
        param.combo.num <- row.names( bakObj$param.combo )

        # test whether we already have that in our 'register'
        if( !(param.combo.num %in% processedCombos$numbers) ) {

            processedCombos$numbers[param.combo.num] <- param.combo.num
            if(returnData) {

                # prevent memory 'blowup' for large batches
                # memory req's for portfolio is 10x the size of orderbook size
                if(saveMemory) {
                    bakObj$portfolio <- NULL
                    bakObj$orderbook <- NULL
                }

                # processedCombos$data[param.combo.num] <- list(results=bakObj)
                # each result shall have a handle the same as a 'porfolio name'
                processedCombos$data[bakObj$portfolio.st] <- list(results=bakObj)
                print(str(bakObj))
            }
            cat(" done\n")
        } else {
            cat(paste0(" discarding ( already existing combo number ", param.combo.num," )\n"))
        }


        # str(object)
        # print(environment())
        # print(i)

        objNum=objNum+1
    }

    cat("getProcessedCombos(): unlisting numbers\n")
    # length(processedCombos)
    print(processedCombos$numbers)
    processedCombos$numbers <- unlist(processedCombos$numbers) # char vector

    cat("getProcessedCombos(): returning processed combos (numbers & data(if that was requested))\n")
    processedCombos
}

# combineStuff() just as QS combines
addCombinedTradeStats <- function(results) {

    fr <- list()

    portfNum <- length(results)

    for(i in 1:portfNum){

        fr <- results[[i]]

        # add copy of tradeStats to summary list for convenience
        if(!is.null(fr$tradeStats))
            results$tradeStats <- rbind(results$tradeStats,
                                        cbind(fr$param.combo,
                                              fr$tradeStats))
    }

    results
}

# < transferred from the 'forked' copy of QS in my github repo  to make
#   this code more compatible with the official QS version >
# XXX This function duplicates what the code in apply.paramset does
# however, if paramsets can come from some external source (as the
# function apply.paramset implies, there might be a reason for such
# function to exist
# ( I personally need this to be able to restart crashed apply.paramsets() )
# Since functions expand.distributions() and apply.constraints() are
# not officially exported, it makes sense either to do so (to export them)
# or to add this utility function here and export _it_ (need feedback on this)
generate.paramsets <- function( strategy.st, paramset.label, nsamples=0 )
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

# Function description:
# getRemainingParamsets generates the full set of paramset combos and
# removes the paramsets already processed, then returns a dataframe of
# remaining paramsets to be processed
#
# strategy -- name of a strategy or strategy object
getRemainingCombos <- function(customParamsets=NULL, strategy,
                                  paramsetLabel, processedComboNums=NULL)
{
    cat("getRemainingCombos(): function entry\n")

    if(is.null(processedComboNums)) {
        cat("getRemainingCombos(): Attention! processedComboNums are NULL, all param combos shall be (re)submitted!\n")
        # stop ("getRemainingCombos(): processedComboNums must be provided")
    } else {
        if(!(class(processedComboNums)=="character"))
            stop ("getRemainingCombos(): processedComboNums must be a character vector")
    }

    cat("getRemainingCombos(): entry checks done\n")

    # strategy <- quantstrat:::must.be.strategy(strategy.st)
    # paramsets <- quantstrat:::generate.paramsets(strategy.st,"SMA")

    # generate all the paramsets as a dataframe (if needed)
    # XXX generate.paramsets does not exist in the official quantstrat
    if(is.null(customParamsets)) {
        cat("getRemainingCombos(): generating the full paramset from strategy\n")
        # allCombos.df <- quantstrat:::generate.paramsets(strategy.st,paramsetLabel) # FIXME: use the internal function _here_
        allCombos.df <- generate.paramsets(strategy.st,paramsetLabel) # FIXME: use the internal function _here_
    } else {
        cat("getRemainingCombos(): got the full paramset as a parameter\n")
        allCombos.df <- customParamsets
    }

    # selection vector - existing combos
    processedCombosSelection <-  row.names(allCombos.df) %in% processedComboNums
    processedCombosSelection

    unprocessedCombos.df <- allCombos.df[!processedCombosSelection,]

    cat("getRemainingCombos(): returning unprocessed combos\n")
    # plug the following dataframe in apply.paramsets() to continue processing
    unprocessedCombos.df # = paramsets
}

# Test of a function getRemainingParamsets
# this function will 'knock out' 3 processedComboNums to test how those
# knocked out combos will appear in the output
# strategy -- name of a strategy or strategy object
test_getRemainingCombos <- function(customParamsets=NULL, strategy, paramsetLabel, processedComboNums=NULL)
{
    if(is.null(processedComboNums))
        stop ("processedComboNums must be provided")

    if(!(class(processedComboNums)=="character"))
        stop ("processedComboNums must be a character vector")

    if(length(processedComboNums)<4)
        stop ("provide a vector of processedComboNums of length greater than 3")

    # generate all the paramsets as a dataframe (if needed)
    # strategy <- quantstrat:::must.be.strategy(strategy.st)
    # paramsets <- quantstrat:::generate.paramsets(strategy.st,"SMA")
    if(customParamsets==NULL) {
        allCombos.df <- quantstrat:::generate.paramsets(strategy.st,paramsetLabel)
    } else {
        allCombos.df <- customParamsets
    }
    if(nrow(allCombos.df)<4)
        stop ("a strategy setup must generate more than 3 param. combos for this test")

    #--------------------------------------------------------------------------|
    # knock out some combos for testing
    combosToRemove <-  sample(x=processedComboNums, size=3)
    whichToRemove <- which(processedComboNums %in% combosToRemove)
    processedComboNums_reduced <- processedComboNums[-whichToRemove]
    #--------------------------------------------------------------------------|

    # selection vector - existing combos
    processedCombosSelection <-  row.names(allCombos.df) %in% processedComboNums_reduced
    processedCombosSelection

    unprocessedCombos.df <- allCombos.df[!processedCombosSelection,]

    rc <- (nrow(unprocessedCombos.df) +
               length(processedComboNums_reduced) == nrow(allCombos.df))

    if(rc) print("PASS")
    else print("FAIL")
}

# submit paramsets (combinations of param's) into the 'comm. channel' for
# processing in apply.paramset()
submitParamset <- function(combos)
{
    #==========================================================================|
    #- < section: internal boilerplate code > ---------------------------------
    #--------------------------------------------------------------------------|
    # ATTENTION!
    # Do NOT use references to internal var's of .robustR.env in main code body!
    #--------------------------------------------------------------------------|
    # This environment is used as a channel of control over calculations
    checkRobustR.env()
    #--------------------------------------------------------------------------|
    # --in-->[_]
    # .robustR.env$backup.func       = backupResult   # function to save backups
    # .robustR.env$backup.jobDir     = "testFailSafe" # netw. path in redisWorker.conf
    # .robustR.env$backup.jobPrefix  = "fubee"   # prefix to find all completed runs
    # .robustR.env$backup.objectName = "result"  # can be used within ANY function
    # .robustR.env$backup.debugFlag  = TRUE      # separate file with extra debug info
    # .robustR.env$redisHost         = "192.168.x.x"  # IP addr. of redis server

    # "applPara" prefix stands for "apply.paramset function args"
    # .robustR.env$applPara.strategy.st    = strategy.st
    # .robustR.env$applPara.paramset.label = paramset.label
    # .robustR.env$applPara.portfolio.st   = portfolio.st
    # .robustR.env$applPara.account.st     = account.st
    # .robustR.env$applPara.mktdata        = mktdata
    # .robustR.env$applPara.nsamples       = nsamples
    # .robustR.env$applPara.user.func      = user.func
    # .robustR.env$applPara.user.args      = user.args
    # .robustR.env$applPara.calc           = calc
    # .robustR.env$applPara.audit          = audit
    # .robustR.env$applPara.packages       = packages
    # .robustR.env$applPara.verbose        = verbose
    # .robustR.env$applPara.verbose.wrk    = verbose.wrk
    .robustR.env$applPara.paramsets        = combos
    # .robustR.env$applPara.ellipsis       = substitute(list(...))[-1] # FIXME
    #--------------------------------------------------------------------------|
    # [_]--out->
    # tmp.dir = .robustR.env$script.commDir    # comm.chnl "robustR <--> fragileR"
    # tmp.file =.robustR.env$script.commFile   # script communic'n file name
    # master.backupPath = .robustR.env$master.backupPath # path as seen by master
    # backup.jobPrefix = .robustR.env$backup.jobPrefix
    #--------------------------------------------------------------------------|
    #==========================================================================|

}

# based on numbered ordered(!) paramsets!
getUnprocessedCombos <- function(master.backupPath, backup.jobPrefix,
                                 paramsets, strategy.st, paramset.label)
{
    # if incoming 'paramsets' are NULL (same as missing)
    # we generate paramsets based on strategy & paramset.label
    # and then kick out combos that exist in the backup

    # if incoming 'paramsets' are not NULL,
    # we simply kick out combos that exist in the backup

    cat("getUnprocessedCombos(): checking whether all the paramsets have been processed\n")

    # just need the numbers of combos here - as vectors
    processedComboNums <- getProcessedCombos(backupPath = master.backupPath,
                                             jobPrefix = backup.jobPrefix,
                                             returnData = FALSE)$numbers


    cat("getUnprocessedCombos(): getting remaining combos:\n")
    # based on ordered(!) paramsets!
    remainingCombos <- getRemainingCombos(customParamsets = paramsets, #if==NULL => generate from strategy!
                                          strategy = strategy.st,
                                          paramsetLabel = paramset.label,
                                          processedComboNums = processedComboNums)

    cat("getUnprocessedCombos(): remaining number of combos to be processed =",
        nrow(remainingCombos),"\n")

    cat("getUnprocessedCombos(): remaining combos:\n")

    print(remainingCombos)

    return(remainingCombos)
}

# Function description:
# robust apply.paramset --> implemented in a separate R process
apply.paramset.r <- robustApplyParamset <-
    function(strategy.st, paramset.label, portfolio.st, account.st,
             mktdata=NULL, nsamples=0, user.func=NULL, user.args=NULL,
             calc='slave', audit=NULL, packages=NULL, verbose=FALSE,
             verbose.wrk=FALSE, paramsets, ...,
             resume_from_backup=FALSE,
             save_memory=TRUE)
{
    #  < debug switch > ----
    if(0) {._DEBUG=TRUE} else {._DEBUG=FALSE}

    if(._DEBUG) {
        cat("apply.paramset.r(): ATTENTION: operating in DEBUG mode !!!")
    }


    #==========================================================================|
    #- < section: internal boilerplate code > ---------------------------------
    #--------------------------------------------------------------------------|
    # ATTENTION!
    # Do NOT use references to internal var's of .robustR.env in main code body!
    #--------------------------------------------------------------------------|
    # This environment is used as a channel of control over calculations
    checkRobustR.env()
    #--------------------------------------------------------------------------|
    # [_]--out->
    tmp.dir = .robustR.env$script.commDir    # comm.chnl "robustR <--> fragileR"
    tmp.file =.robustR.env$script.commFile   # script communic'n file name
    master.backupPath = .robustR.env$master.backupPath # path as seen by master
    backup.jobPrefix  = .robustR.env$backup.jobPrefix
    backup.objectName = .robustR.env$backup.objectName # can be used within ANY function
    output.objectName = .robustR.env$output.objectName  # final combined object name
    #--------------------------------------------------------------------------|
    # --in-->[_]
    # .robustR.env$backup.func       = backupResult   # function to save backups
    # .robustR.env$backup.jobDir     = "testFailSafe" # netw. path in redisWorker.conf
    # .robustR.env$backup.jobPrefix  = "fubee"   # prefix to find all completed runs
    # .robustR.env$backup.objectName = "result"  # can be used within ANY function
    # .robustR.env$backup.debugFlag  = TRUE      # separate file with extra debug info
    # .robustR.env$redisHost         = "192.168.x.x"  # IP addr. of redis server

    # "applPara" prefix stands for "apply.paramset function args"
    .robustR.env$applPara.strategy.st    = strategy.st
    .robustR.env$applPara.paramset.label = paramset.label
    .robustR.env$applPara.portfolio.st   = portfolio.st
    .robustR.env$applPara.account.st     = account.st
    .robustR.env$applPara.mktdata        = mktdata
    .robustR.env$applPara.nsamples       = nsamples
    .robustR.env$applPara.user.func      = user.func
    .robustR.env$applPara.user.args      = user.args
    .robustR.env$applPara.calc           = calc
    .robustR.env$applPara.audit          = audit
    .robustR.env$applPara.packages       = packages
    .robustR.env$applPara.verbose        = verbose
    .robustR.env$applPara.verbose.wrk    = verbose.wrk
    .robustR.env$applPara.save_memory    = save_memory
    # .robustR.env$applPara.paramsets      = paramset_full # XXX <- check this later #############################
    .robustR.env$applPara.ellipsis       = substitute(list(...))[-1] # FIXME: ellipsis ----
    #--------------------------------------------------------------------------|
    #==========================================================================|


    #--------------------------------------------------------------------------|
    # set the user-set scope of param's to determine the final scope of output
    if(missing(paramsets)||is.null(paramsets)) {
        cat("paramsets argument is NULL (or missing),",
            "generating from strategy and assigning to paramset_full variable\n")
        paramset_full <- generate.paramsets(strategy.st = strategy.st,
                                            paramset.label = paramset.label,
                                            nsamples = nsamples)
    } else {
        cat("paramsets argument has been provided,",
            "assigning to paramset_full variable\n")
        paramset_full <- paramsets
    }

    #--------------------------------------------------------------------------|
    # < pre-processing args to pass them to the internal comm. channel > ----
    if(resume_from_backup) {
        cat("resuming from backup: paramset_wrk <- getUnprocessedCombos\n")
        # find the diff. between paramsets (if present) or generated set of combos
        # and saved paramsets => submit them for further processing
        paramset_wrk <- getUnprocessedCombos(master.backupPath = master.backupPath,
                                             backup.jobPrefix  = backup.jobPrefix,
                                             paramsets         = paramset_full,
                                             strategy.st       = strategy.st,
                                             paramset.label    = paramset.label)

    } else {
        cat("starting from scratch: paramset_wrk <- paramset_full\n")
        paramset_wrk <- paramset_full
    }

    cat("paramsets to process: (paramset_wrk) \n")
    cat("---- list start ----\n")
    print(paramset_wrk)
    cat("---- list end ------\n")

    cat("setting the working set of parameters to process (pushing paramset_wrk to internal comm. channel)\n")
    submitParamset(paramset_wrk)
    #--------------------------------------------------------------------------|


    # save all the needed objects in an .RData file and launch the script with
    # a regular apply strategy + a check that all the paramsets have been
    # found in the backup folder

    ###########################################################################|
    # packing the environments:
    # not using standard ls/get/put to make sure _everything_ is available
    # the easiest solution is to simply dump everything!
    if(0) {
        ls(envir = FinancialInstrument:::.instrument,all.names = TRUE)

        ls(envir = .blotter,all.names = TRUE)

        ls(envir = .strategy,all.names = TRUE)
    }

    # defined _before_ saving the workspace so the script knows where it is
    # (just in case)
    if(._DEBUG) {
        scriptFileFullPath <-
            "e:/devt/aa_my_github/rfintools/exec/robustApplyParamsetScript.R"
    } else {
        scriptFileFullPath <- paste0(path.package("rfintools"),
                                     "/exec/robustApplyParamsetScript.R")
    }

    # get data from the script out of this file:
    scriptOutputFileFullPath <- paste0(tmp.dir,"/",tmp.file)

    # pass workspace via a file in the temp folder
    workspaceFileFullPath <- tempfile()



  ##==loop start-->
    numberOfRestarts <- 0L
    calcComplete <- FALSE
    criticalFailure <- FALSE

    if(resume_from_backup) {
        neverFailed <- FALSE
    } else { neverFailed <- TRUE }

    while ( (!calcComplete) && (!criticalFailure) ) {

        # do not save the current workspace inside the loop: TODO: why did I write this ????? ----

        # to be loaded using inside the script within the loop
        rc <- saveWorkspace(workspaceFileFullPath) # rc==0 == 'ok' critical error otherwise

        if(rc!=0) {
            criticalFailure <- TRUE
            cat("criticalFailure!\n")
        }

        if(!criticalFailure)  {
            #----------------------------------------------------------------------|
            # run script which will save its result / output in a .RData file
            # (to be read after script has finished working)
            rc <- system2(command="Rscript",
                          args=c(scriptFileFullPath,
                                 workspaceFileFullPath,
                                 scriptOutputFileFullPath), # pass thru cmdLine args
                          wait = TRUE
            )#, scriptSetupFile, scriptOutputFile))

            cat("control returned to the main ('crash-safe') master process\n")
            cat("script exit code was",rc,"\n")

            if(rc==127) {
                cat("criticalFailure: script could not be run\n")
                criticalFailure <- TRUE
                neverFailed <- FALSE
            }

            if(rc!=0) { neverFailed <- FALSE }
            #----------------------------------------------------------------------|
        }

        # if this is the first iteration & we're done, just get out of the loop
        if((!criticalFailure) && (neverFailed)) {
            cat("we're done 'looping' after just one pass!\n")
            calcComplete <- TRUE
        }

        # if no critical failure occurred AND there was @ least one script crash
        if((!criticalFailure) && (!neverFailed)) {

            remainingCombos <- getUnprocessedCombos(master.backupPath,
                                                    backup.jobPrefix,
                                                    paramsets=paramset_full,
                                                    strategy.st,
                                                    paramset.label)

            if( nrow(remainingCombos)==0 ) {
                calcComplete <- TRUE
            }

            # submit the remaining paramsets, just in case
            submitParamset(remainingCombos) # into the 'comm. channel'

        }

        # if((!criticalFailure) && (!calcComplete)) {
        #
        # }


    }
  ##==loop end-->   >--at this point, all the work has been saved on disk--<

    if(!criticalFailure) {
        # debug mode always recompiles results from backup
        if(neverFailed && (!._DEBUG) ) {
        ##==if we don't need to combine backups to get result -->

            cat("Loading data from the script from ", scriptOutputFileFullPath, "\n")

            # get 'results' object
            load(file=scriptOutputFileFullPath, verbose = TRUE)
            # TODO: error checking with an external function ----

            returnValue <- get(output.objectName)#, envir = globalenv() ) # from the file loaded

        } else {
        ##==if we do need to combine backups to get result -->

            # FIXME: this might be limited by incoming paramsets
            # (i.e. if resume from backup option is active, but paramsets are also given !)

            # read backups
            processedComboResults <- getProcessedCombos(backupPath  = master.backupPath,
                                                        jobPrefix   = backup.jobPrefix,
                                                        returnData  = TRUE,
                                                        saveMemory = save_memory)$data

            # XXX TODO: limit processed combo results by the scope: ----
            # paramset_full (the function should not
            # return more than a user requests, this may be quite frequently
            # the case when a user 'resumes' the job with a smaller
            # paramset !!!)
            # NOT YET IMPLEMENTED HERE !!! TODO !!!

            # demonstrate - for debugging only
            print(str(processedComboResults))

            # combineStuff() just as QS combines
            returnValue <- addCombinedTradeStats(processedComboResults)


            returnValue
        }
    } else {

        cat("critical failure\n")

        returnValue <- master.backupPath # see backup data here

    }

    if(0) {
        allDone <- FALSE
        processedCombos <- getProcessedComboNums(backupPath = master.backupPath,
                                                 jobPrefix = backup.jobPrefix)
        remainingCombos <- getremainingCombos(strategy = strategy.st,
                                                    paramsetLabel = paramset.label,
                                                    processedCombos = processedCombos)
        if( nrow(remainingCombos)==0 ) { allDone <- TRUE}
        if(!allDone) {
            cat("submitting the remaining combinations...\n")

            # the latest 'results' object and the ones that will follow
            # shall be discarded
            # as only the final 'combine' operation will produce that required
            # 'results' object
        }
        #----------------------------------------------------------------------|
        # run the script again
        #----------------------------------------------------------------------|
        if(!allDone) {
            # 'combine' the results into one
            # borrow the code from QuantStrat
        }
    }

    cat("deleting the initial workspace from disk\n")
    # delete the file with the workspace
    if (file.exists(workspaceFileFullPath)) file.remove(workspaceFileFullPath)


    cat("returning the result\n")
    # the same output as would be produced by the apply.paramset() w/o crashing
    # results <- returnValue # FIXME (use output.objectName with assign("results", etc. etc.) )
    return(returnValue)
}



## < section: sandbox area > ---------------------------------------------------
if(0) {
    # getComboJobFiles()

    processedCombos <- list()
    processedCombos <-
    getProcessedComboNums( backupPath="//host/d-sto-SINK/testFailSafe",
                                       jobPrefix="fub3",
                                       verbose=FALSE)

    processedCombos

    # knock out some combos for testing
    if(0) {
        combosToRemove <-  sample(x=processedCombos, size=3)
        whichToRemove <- which(processedCombos %in% combosToRemove)
        processedCombos <- processedCombos[-whichToRemove]
    }

    # simulated result after imaginary crash:
    processedCombos

    allCombos.df <- getRemainingComboss(strategy="sma1", paramsetLabel="SMA", processedCombos=processedCombos)
    # str(allCombos) -- dataframe
    nrow(allCombos.df)

    # selection vector - existing combos
    processedCombosSelection <-  row.names(allCombos.df) %in% processedCombos
    processedCombosSelection

    unprocessedCombos.df <- allCombos[!processedCombosSelection,]
    unprocessedCombos.df


}

## < section: installation shortcuts, etc. > -----------------------------------

# Tests (to be moved to /tests/ some day, hopefully):
if(0) {
    processedCombos <-
        getProcessedCombos( backupPath="//host/d-sto-SINK/testFailSafe",
                               jobPrefix="fub1",
                               verbose=FALSE,
                               returnData=FALSE)$numbers
    processedCombos <-
        getProcessedCombos( backupPath="//host/d-sto-SINK/testFailSafe",
                               jobPrefix="fub1",
                               verbose=FALSE,
                               returnData=TRUE)

    test_getRemainingParamsets(strategy="sma1", paramsetLabel="SMA", processedCombos=processedCombos)

}

if(0) {
    library(devtools)
    install_local("e:/devt/aa_my_github/rfintools", keep_source=TRUE)
    install_local("e:/devt/aa_my_github/quantstrat", keep_source=TRUE)
}



