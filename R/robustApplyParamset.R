
# Author: cloudcello
# Date: 2015-12-23

# ##############################################################################
# Reference:
#
# finding a path of a souced file:
#    http://stackoverflow.com/questions/3452086/getting-path-of-an-r-script
#    Dirk argues that creating a package is the way to go
#
# creating folders with shared read/write permissions
#    http://superuser.com/questions/280994/give-write-permissions-to-multiple-users-on-a-folder-in-ubuntu
# ##############################################################################

# Description:
# The usual modus operandi: run - fail - retrieve all saved data - reconstruct
# the list of missing data - create new paramset in a temporary strategy
# object - run again until completion

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

# Configuration files:
#    Config. files must have the following format ("<-space(s)->" == "\s+")
#    HOSTNAME <-space(s)-> ANY_REACHEABLE_PATH_TO_SHARED_DIRECTORY
#    Host names must always be written in the capital case
#    Tildas in path are not allowed (no tilda expansion is performed)

# ATTN!
#    row.names(param.combo) can be used only within quantstrat apply.paramset
#    FIXME! get this combo name from the 'result' if possible

# Function description:
#    A user function for using with apply.paramset
#    to save backups of processed tasks
backupResult <- function(cfgFile="redisWorker.conf",
                         # the cfgFile must be located in the same folder
                         # as the worker script. Currently R has no function
                         # to determine the location of the source unless
                         # the file is run through Rscript
                         # A good alternative is to create a package (as Dirk
                         # suggested at StackOverflow) and get a relative
                         # location based on the location of the package.

                         # The name of the folder does not mean
                         # 'for this specific job' - for multiple jobs
                         # should be more properly be called 'backupDir'
                         # This dir. must be created manually
                         # with group permissions common to both
                         # external and internal users
                         jobDir="testFailSafe", # FIXME: rename to 'backupDir'

                         jobPrefix="foo", # in case files from multiple jobs are
                                       # saved into the same folder

                         objectName=NULL,
                         comboName=row.names(param.combo),
                         debugFlag=FALSE)
{
    cat("backupResult(): function entery.\n")
    cat(
        paste0("host:",Sys.info()["nodename"],
               "-pid:",Sys.getpid(),
               " workdir: ", getwd(),
               "\n")
    )


    ###########################################################################
    # TODO this must be done only once (perhaps even at the level of the
    #      script rather than this function!!! set some variable in the
    #      worker's global(?) environment to keep any needed info from the
    #      config until the next 'job' is sent from the master process
    ###########################################################################
    cat("backupResult(): looking for a cfg file.\n")
    if(!file.exists(cfgFile)) {
        cat("Worker config file not found in the working directory.\n")
        stop( "Worker config file not found in the working directory." )
    } else {
        cat("backupResult(): found cfg file.\n")
    }

    cfgText <- readLines(cfgFile)
    cat("backupResult(): read cfg file.\n")

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
        cat( paste0("Worker cannot access the specified backup directory:\n",
                    backupPathFull,"\n",
                  "Make sure it has been created by the master process.") )
        quit(save="no", runLast = FALSE)
    }

    # save(backupPath,file=paste0(backupPath,"/","testBBOX.RData"))
    cat("backupResult(): full backup path has been set.\n")
    ###########################################################################
    # Now the worker has a path set
    ###########################################################################


    cat("backupResult(): checking for uniqueness of a file name.\n")
    # check whether the file to be written already exists. If so, save the file
    # with a unique suffix via "DUP" & tempfile()

    # comboName="1"
    prefixedComboName <- paste0(jobPrefix, comboName)

    baseFileName = paste0(prefixedComboName,".RData")

    cat(paste0("Backup data file [combo.number].RData = ", baseFileName, "\n"))

    fullPathAndFileName = paste0(backupPathFull, "/", baseFileName)

    cat(paste0(" Checking whether baseFileName = ", baseFileName, " already exists: "))

    # assign a "duplicate" file name for this worker output
    if(file.exists(fullPathAndFileName)) {
        cat("Yep!\n")
        cat(paste0("baseFileName = ", baseFileName, " already exists.\n"))
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

        cat(paste0("Saving data as a duplicate [ ", baseFileName, " ]\n"))
    } else {
        cat("Nope!\n") # no file with the same name
    }

    # save(list=objectName, file="//server/data_01/aa_cluster_backups/dummy_var.RData")
    # dummyobj <- "dummy"
    # objectName <- "dummyobj"
    cat("Saving backup...\n")
    cat("What: ",objectName,"\n" )
    cat("Where: ",fullPathAndFileName,"\n" )

    rc <- try(
        save(list=objectName, file=fullPathAndFileName ),
        silent=TRUE
    )

    if(is.null(rc)) {
        cat("Backup saved on ", date(),"\n")
    } else {
        print(rc)
    }

    if(debugFlag) {
        debugData <- list(sysInfo=Sys.info(),pID=Sys.getpid())
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
            cat("Debug data saved on ", date(),"\n")
        } else {
            print(rc)
        }
    }

    return(0)
}

# may be deleted ---------------------------------------------------------------

if(0){ # testing
    # srcDir <- dirname(parent.frame()$ofile)
    # str(parent.frame())
    # getSrcDirectory(function(x) {x})
    # script.dir <- dirname(sys.frame(1)$ofile)
    owd <- getwd()
    setwd("c:/R/redis-worker-scripts")


    ###########################################################################
    # TODO this must be done only once! set some variable to keep
    #      any needed info from the config until the next 'job' is
    #      sent from the master process
    ###########################################################################
    if(!file.exists(cfgFile)) {
        stop( "Worker config file not found in the working directory." )
    }

    cfgText <- readLines(cfgFile)

    # regexp:
    # {n,} The preceding item is matched n or more times.
    # + The preceding item will be matched one or more times.

    parsed1 <- strsplit(cfgText, split=' +' )
    parsed2 <- list()
    for(i in seq(length(parsed1))) {
        parsed2[parsed1[[i]][1]] <- parsed1[[i]][2]
    }
    backupPath <- unlist(parsed2[Sys.info()["nodename"]])
    save(backupPath,file=paste0(backupPath,"/","testBBOX.RData"))


    unlist(parsed1)
    parsed1[[1]]
    a <- list()
    a <-
    lapply(parsed1, a[parsed[1]])
    #

    Sys.info()["nodename"]


    cat("we're in ",  script.dir , "\n")
    setwd(this.dir)

    cfgFile="~/.redisWorker.conf"
    readLines(cfgFile)
    getwd()


    baseDir="//server/data_01/aa_cluster_backup"
    jobDir="testFailSafe"
    backupPath <- paste0(baseDir,"/", jobDir)
    comboName <- 1
    cat(paste0("Backup data file [combo.number].RData = ", comboName, ".Rdata\n"))
    baseFileName = paste0(comboName,".RData")
    fullPathAndFileName = paste0(backupPath, "/", baseFileName)

    # name this worker output as a duplicate
    if(file.exists(fullPathAndFileName)) {
        cat(paste0("baseFileName = ", baseFileName, " already exists.\n"))
        fullPathAndFileName <- tempfile(pattern=paste0(comboName,"_DUP_"), tmpdir=backupPath, fileext=".RData")

        baseFileName <- substr(fullPathAndFileName, start = nchar(backupPath)+1L, stop = nchar(fullPathAndFileName))

        # Workers running on Windows will have tempfile() produce a path with a backslash (as "\\")
        # so 1 extra character must be dealt with separately:
        if(substr(baseFileName,1,1)=="\\") baseFileName <- substr(baseFileName,2,nchar(baseFileName))

        cat(paste0("Saving data as a duplicate [ ", baseFileName, " ]\n"))
    }
    debugData <- list(sysInfo=Sys.info(),pID=Sys.getpid())
    # rm(DEBUGDATA)
    # DEBUGDATA$sysInfo["nodename"]
}

if(0) {
    # Linux server's processes do not have access to folders created
    # with dir.create() through samba (from the outside)

    tmpWD <- getwd() # TODO consider restoring the working directory

    # hostNameRemoteDir <- paste0("//",hostName,"/",remoteDir)
    cat("setwd() to ", hostNameRemoteDir, "\n")
    setwd(hostNameRemoteDir)

    check_host_rdir <-  getwd()
    cat(paste0("CHECK: setwd() to", check_host_rdir, "\n"))
}


# if(!dir.exists(backupPathFull)){
#     # Caution: use only one folder level ("flat is better than nested" rule)
#     dir.create(backupPathFull, recursive = TRUE)#, mode = "0777")
# }
