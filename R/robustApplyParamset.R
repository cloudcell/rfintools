

### THIS FUNCTION BELONGS IN THE FOREACH PACKAGE (IF I MANAGE TO SUPPLY THE
### COMBO NUMBER TO THIS FUNCTION, if that's really necessary.)

# Description:
# The usual modus operandi: run - fail - retrieve all saved data - reconstruct
# the list of missing data - create new paramset in a temporary strategy
# object - run again until completion

## A user function for using with apply.paramset
## to sabe backups of processed tasks
backupResult <- function(hostName=NULL, remoteDir=NULL, jobSubDir=NULL, objectName=NULL, debugFlag=FALSE)
{
    if(is.null(remoteDir)) {
        # no hostName / remoteDir means the path is local to worker's working
        # directory -- not yet supported
        stop ("Please provide base backup folder.")
    }

    # This is a trick that deals with the function's dir.create() attempt (only
    # on linux machines) to create the folder based on the "host" part of the
    # path when given //host/remoteDir/subfolder
    # Such attempt causes a failure at the combine stage of the foreach (for
    # a yet unknown to me reason).

    tmpWD <- getwd() # TODO consider restoring the working directory

    setwd(paste0("//",hostName,"/",remoteDir))

    backupPath <- paste0("./", jobSubDir)

    if(!dir.exists(backupPath)){
        # Caution: use only one folder level ("flat is better than nested" rule)
        dir.create(backupPath, recursive = TRUE)#, mode = "0777")
    }

    # check whether the file to be written already exists. If so, save the file
    # with a unique suffix via "DUP" & tempfile()

    ### ATTN! row.names(param.combo) can be used only within quantstrat apply.paramset
    ### FIXME! get this combo name from the 'result' if possible
    comboName <- row.names(param.combo)
    cat(paste0("Backup data file [combo.number].RData = ", comboName, ".Rdata\n"))
    baseFileName = paste0(comboName,".RData")
    fullFileName = paste0(backupPath, "/", baseFileName)

    # assign a "duplicate" file name for this worker output
    if(file.exists(fullFileName)) {
        cat(paste0("baseFileName = ", baseFileName, " already exists.\n"))
        fullFileName <- tempfile(pattern=paste0(comboName,"_DUP_"), tmpdir=backupPath, fileext=".RData")

        baseFileName <- substr(fullFileName, start = nchar(backupPath)+1L, stop = nchar(fullFileName))

        # Workers running on Windows will have tempfile() produce a path with a backslash (as "\\")
        # so 1 extra character must be dealt with separately:
        if(substr(baseFileName,1,1)=="\\") baseFileName <- substr(baseFileName,2,nchar(baseFileName))

        cat(paste0("Saving data as a duplicate [ ", baseFileName, " ]\n"))
    }

    # save(list=objectName, file="//server/data_01/aa_cluster_backups/dummy_var.RData")
    save(list=objectName, file=fullFileName )
    cat("Backup saved\n")

    if(debugFlag) {
        debugData <- list(sysInfo=Sys.info(),pID=Sys.getpid())
        debugTag <- paste("_DEBUG", debugData$sysInfo["nodename"], debugData$pID,sep = "_")
        save(list="debugData", file=paste0(fullFileName,debugTag,".RData") )
        cat("Debug data saved\n")
    }

    return(0)
}

if(0){ # testing
    baseDir="//server/data_01/aa_cluster_backup"
    jobSubDir="testFailSafe"
    backupPath <- paste0(baseDir,"/", jobSubDir)
    comboName <- 1
    cat(paste0("Backup data file [combo.number].RData = ", comboName, ".Rdata\n"))
    baseFileName = paste0(comboName,".RData")
    fullFileName = paste0(backupPath, "/", baseFileName)

    # name this worker output as a duplicate
    if(file.exists(fullFileName)) {
        cat(paste0("baseFileName = ", baseFileName, " already exists.\n"))
        fullFileName <- tempfile(pattern=paste0(comboName,"_DUP_"), tmpdir=backupPath, fileext=".RData")

        baseFileName <- substr(fullFileName, start = nchar(backupPath)+1L, stop = nchar(fullFileName))

        # Workers running on Windows will have tempfile() produce a path with a backslash (as "\\")
        # so 1 extra character must be dealt with separately:
        if(substr(baseFileName,1,1)=="\\") baseFileName <- substr(baseFileName,2,nchar(baseFileName))

        cat(paste0("Saving data as a duplicate [ ", baseFileName, " ]\n"))
    }
    debugData <- list(sysInfo=Sys.info(),pID=Sys.getpid())
    # rm(DEBUGDATA)
    # DEBUGDATA$sysInfo["nodename"]
}



