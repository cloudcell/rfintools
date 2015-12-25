
################################################################################
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
# Acknowledgements
# Some code borrowed from the following sources:
#  * ...

################################################################################
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


################################################################################
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

################################################################################
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

# TODO: make a utility function for printig debug data as follows:
#       {function name}(): {message}

################################################################################
# Function description:
#    A user function for using with apply.paramset()
#    to save backups of processed tasks to a specified location

# cfgFile The config file must be located in the same folder as the worker
# script. Currently R has no function to determine the location of the source
# unless the file is run through Rscript A good alternative is to create a
# package (as Dirk suggested at StackOverflow) and get a relative location based
# on the location of the package.

# jobDir Directory within the shared location (defined in cfgFile). The purpose
# of jobDir is to arbitrarily seggregate backup files The name of the folder
# does not mean a folder 'for this specific job' - for multiple jobs should be
# more properly be called 'backupDir' This dir. must be created manually with
# group permissions common to both external and internal users

# jobPrefix in case files from multiple jobs are saved into the same folder

# objectName a name of an object to be saved
# debugFlag to save additional debug info in a separate file, default FALSE

backupResult <- function(cfgFile="redisWorker.conf",
                         jobDir="testFailSafe", # XXX: might be renamed to 'backupDir'
                         jobPrefix="foo",
                         objectName="result",
                         debugFlag=FALSE
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

    ###########################################################################
    # TODO this must be done only once (perhaps even at the level of the
    #      script rather than this function!!! set some variable in the
    #      worker's global(?) environment to keep any needed info from the
    #      config until the next 'job' is sent from the master process
    ###########################################################################
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
    ###########################################################################
    # Now the worker has a path set
    ###########################################################################

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
        print(bakObj)
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

if(0){
    a <- list()
    is.list(a)
    str(a)
    attr(a)
}

# Function Description:
# reads backup files into memory & retrieves combo numbers into a list
getProcessedComboNums <- function( backupPath="//host/shared/jobDir",
                                   jobPrefix="foo",
                                   verbose=FALSE)
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
    processedCombos <- list()
    objNum=1
    for(i in comboJobFiles) {

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

        processedCombos[objNum] <- param.combo.num

        # str(object)
        # print(environment())
        # print(i)

        objNum=objNum+1
    }

    # length(processedCombos)
    unlist(processedCombos) # char vector
}

# Function description:
# getRemainingParamsets generates the full set of paramset combos and
# removes the paramsets already processed, then returns a dataframe of
# remaining paramsets to be processed
#
# strategy -- name of a strategy or strategy object
getRemainingParamsets <- function(strategy, paramsetLabel, processedCombos=NULL)
{
    if(is.null(processedCombos))
        stop ("processedCombos must be provided")

    if(!(class(processedCombos)=="character"))
        stop ("processedCombos must be a character vector")

    # strategy <- quantstrat:::must.be.strategy(strategy.st)
    # paramsets <- quantstrat:::generate.paramsets(strategy.st,"SMA")

    # generate all the paramsets as a dataframe
    # XXX generate.paramsets does not exist in the official quantstrat
    allCombos.df <- quantstrat:::generate.paramsets(strategy.st,paramsetLabel)

    # selection vector - existing combos
    processedCombosSelection <-  row.names(allCombos.df) %in% processedCombos
    processedCombosSelection

    unprocessedCombos.df <- allCombos.df[!processedCombosSelection,]

    # plug the following dataframe in apply.paramsets() to continue processing
    unprocessedCombos.df # = paramsets
}


# Test of a function getRemainingParamsets
# this function will 'knock out' 3 processedCombos to test how those
# knocked out combos will appear in the output
# strategy -- name of a strategy or strategy object
test_getRemainingParamsets <- function(strategy, paramsetLabel, processedCombos=NULL)
{
    if(is.null(processedCombos))
        stop ("processedCombos must be provided")

    if(!(class(processedCombos)=="character"))
        stop ("processedCombos must be a character vector")

    if(length(processedCombos)<4)
        stop ("provide a vector of processedCombos of length greater than 3")

    # strategy <- quantstrat:::must.be.strategy(strategy.st)
    # paramsets <- quantstrat:::generate.paramsets(strategy.st,"SMA")
    # generate all the paramsets as a dataframe
    allCombos.df <- quantstrat:::generate.paramsets(strategy.st,paramsetLabel)
    if(nrow(allCombos.df)<4)
        stop ("a strategy setup must generate more than 3 param. combos for this test")

    #---------------------------------------------------------------------------
    # knock out some combos for testing
    combosToRemove <-  sample(x=processedCombos, size=3)
    whichToRemove <- which(processedCombos %in% combosToRemove)
    processedCombos_reduced <- processedCombos[-whichToRemove]
    #---------------------------------------------------------------------------

    # selection vector - existing combos
    processedCombosSelection <-  row.names(allCombos.df) %in% processedCombos_reduced
    processedCombosSelection

    unprocessedCombos.df <- allCombos.df[!processedCombosSelection,]

    rc <- (nrow(unprocessedCombos.df) +
               length(processedCombos_reduced) == nrow(allCombos.df))

    if(rc) print("PASS")
    else print("FAIL")
}


robustApplyParamset <- function(strategy.st, paramset.label, portfolio.st,
                                account.st, mktdata=NULL, nsamples=0,
                                user.func=NULL, user.args=NULL, calc='slave',
                                audit=NULL, packages=NULL, verbose=FALSE,
                                verbose.wrk=FALSE, paramsets, ...)
{

    # save all the arguments in an .RData file and launch the script with
    # a regular apply strategy + a check that all the paramsets have been
    # found in the backup folder
    user.func = backupResult
    user.args = list(jobDir="testFailSafe", # does not mean 'for this specific job' - for multiple jobs
                     # should be more properly be called 'backupDir'
                     objectName='result',
                     jobPrefix="fub5",
                     debugFlag=TRUE
                     # result=result,
                     # param.combo=param.combo
    )
    packages=c("rfintools")

    paramset.label="SMA"
    strategy.st="sma1"
    mktdata=NULL
    nsamples=0
    calc='slave'
    audit=NULL
    verbose=FALSE
    verbose.wrk=FALSE
    # paramsets
    ...=list(abra="cadabra")
    list(...)

    applyStrategyArgs = list(strategy.st=strategy.st,
                             paramset.label=paramset.label,
                             portfolio.st=portfolio.st,
                             account.st=account.st,
                             mktdata=mktdata,
                             nsamples=nsamples,
                             user.func=user.func,
                             user.args=user.args,
                             calc=calc,
                             audit=audit,
                             packages=packages,
                             verbose=verbose,
                             verbose.wrk=verbose.wrk
                             # paramsets=paramsets,
                             # ...
                             )

    scriptSetupFile="robustApplyParamsetParams.RData"
    # XXX strategy object must be exported too !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    strategyName <- "sma1"
    strategyToExport <- getStrategy(strategyName)
    # get.strategy(x, envir = .strategy) --> create envir ".strategy" inside

    # then just supply the strategy itself instead of a name ! NO! must be
    # exported properly because getStrategy is used inside apply.paramset!
    str(strategyToExport)
    save("applyStrategyArgs","strategyToExport","strategyName", file=scriptSetupFile )



    getwd()
    scriptFileFullPath <- paste0(path.package("rfintools"),"/R/robustApplyParamsetScript.R")
    scriptFileFullPath <- "e:/devt/aa_my_github/rfintools/R/robustApplyParamsetScript.R"
    # run script which will save its result / output in a .RData file
    # (to be read after script has finished working)
    system2(command="Rscript",args=c(scriptFileFullPath, scriptSetupFile))

    if(0) {
        # the following apply.paramset must be run in a separate R process
        # which can crash and be restarted automatically

        results <- apply.paramset(strategy.st=strategy.st,
                                  paramset.label=paramset.label,
                                  portfolio.st=portfolio.st,
                                  account.st=account.st,
                                  mktdata=mktdata,
                                  nsamples=nsamples,
                                  user.func=user.func,
                                  user.args=user.args,
                                  calc=calc,
                                  audit=audit,
                                  packages=packages,
                                  verbose=verbose,
                                  verbose.wrk=verbose.wrk,
                                  paramsets=paramsets,
                                  ...)

        result <- warning(... = 1)
        attr(result,which = "2")
        str(result)
    }

    # the same output as would be produced by the apply.paramset() w/o crashing
    results
}

# sandbox area -----------------------------------------------------------------
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

    allCombos.df <- getRemainingParamsets(strategy="sma1", paramsetLabel="SMA", processedCombos=processedCombos)
    # str(allCombos) -- dataframe
    nrow(allCombos.df)

    # selection vector - existing combos
    processedCombosSelection <-  row.names(allCombos.df) %in% processedCombos
    processedCombosSelection

    unprocessedCombos.df <- allCombos[!processedCombosSelection,]
    unprocessedCombos.df


}

# installation shortcuts, etc. -------------------------------------------------

# Tests (to be moved to /tests/ some day, hopefully):
if(0) {
    processedCombos <-
        getProcessedComboNums( backupPath="//host/d-sto-SINK/testFailSafe",
                               jobPrefix="fub3",
                               verbose=FALSE)
    test_getRemainingParamsets(strategy="sma1", paramsetLabel="SMA", processedCombos=processedCombos)

}

if(0) {
    library(devtools)
    install_local("e:/devt/aa_my_github/rfintools", keep_source=TRUE)
    install_local("e:/devt/aa_my_github/quantstrat", keep_source=TRUE)
}



