# temporary place for code testing  
# sources: StackOverflow posts   (my modifications)

getScriptPath <- function(){
    cmd.args <- commandArgs()
    m <- regexpr("(?<=^--file=).+", cmd.args, perl=TRUE)
    script.dir <- dirname(regmatches(cmd.args, m))
    if(length(script.dir) == 0) stop("can't determine script dir: please call the script with Rscript")
    if(length(script.dir) > 1) stop("can't determine script dir: more than one '--file' argument detected")
    return(script.dir)
}


print("getScriptPath:")
cat(getScriptPath(),"\n")


# script.dir <- dirname(sys.frame(1)$ofile)
print("trick directory:")
getSrcDirectory(function(x) {x})
print("working directory:")
print(getwd())

script.dir <- dirname(parent.frame(2)$ofile)
cat("we're in ",  script.dir , "\n")

system.file()
R.home()
list.files()
