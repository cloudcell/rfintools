
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

################################################################################
# Reference:
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
#    HOSTNAME <-space(s)-> ANY_REACHEABLE_PATH_TO_SHARED_DIRECTORY
#    Host names must always be written in the capital case
#    Tildas in path are not allowed (no tilda expansion is performed)
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
    cat(" * object to be saved: ",objectName,"\n" )
    cat(" * backup location: ",fullPathAndFileName,"\n" )

    # get the object from the environment of the _calling_ function
    # i.e. using the 'call chain' (parent.frame())
    # NOT the 'environment chain' (parent.env()) !
    object <- get(envir=env,objectName)
    rc <- try(
        save(list="object", file=fullPathAndFileName ),
        silent=TRUE
    )

    if(is.null(rc)) {
        cat("backupResult(): backup saved on ", date(),"\n")
    } else {
        print(rc)
    }

    if(debugFlag) {
        debugData <- list(sysInfo=Sys.info(),pID=Sys.getpid(), comboName=comboName, jobPrefix=jobPrefix)
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

getBackupFileList <- function( backupPath="/fooBar",
                               jobPrefix="foo")
{
    path=backupPath
    jobPrefix="foo"
    path="//host/shared/testFailSafe"
    pattern=paste0(jobPrefix,"-","*.RData")
    rxPattern=glob2rx(pattern)
    myFiles <- list.files(path=path, pattern=rxPattern)
    read.delim
    ls
    return (0)
}

# installation shortcuts, etc. -------------------------------------------------

if(0) {
    library(devtools)
    install_local("e:/devt/aa_my_github/rfintools", keep_source=TRUE)
    install_local("e:/devt/aa_my_github/quantstrat", keep_source=TRUE)
}

