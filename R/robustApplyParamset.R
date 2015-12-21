
## A user function for using with apply.paramset
## to sabe backups of processed tasks
backupResult <- function(baseDir=NULL, jobSubDir=NULL, objectName=NULL)
{
    if(is.null(baseDir)) {
        # no baseDir means the path is local to worker's working directory
        # not yet supported
        stop ("Please provide base backup folder.")
    }

    backupPath <- paste0(baseDir,"/", jobSubDir)

    if(!dir.exists(backupPath)){
        dir.create(backupPath, recursive = TRUE, mode = "0777")
    }

    # check whether the file to be written already exists, if so, save a file with a unique suffix via "DUP" & tempfile()
    ### ATTN! row.names(param.combo) can be used only within quantstrat apply.paramset
    ### FIXME! get this combo name from the 'result' if possible
    comboName <- row.names(param.combo)
    baseFileName = paste0(comboName,".RData")

    # name this worker output as a duplicate
    if(!file.exists(paste0(backupPath,"/",baseFileName))) {
        baseFileName <- tempfile(pattern=paste0(comboName,"_DUP_"), tmpdir=backupPath, fileext=".RData")
    }

    # save(list=objectName, file="//server/data_01/aa_cluster_backups/dummy_var.RData")
    save(list=objectName, file=paste0(backupPath,"/",baseFileName) )

}




