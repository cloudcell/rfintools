
# Authors: see each function's description


# is.ALL has been written by Martin Maechler.
#' @export
is.ALL <- function(obj, func.names = ls(pos=length(search())),
         not.using = c("is.single", "is.real", "is.loaded",
                       "is.empty.model", "is.R", "is.element", "is.unsorted"),
         true.only = FALSE, debug = FALSE)
{
    ## Purpose: show many 'attributes' of  R object __obj__
    ## -------------------------------------------------------------------------
    ## Arguments: obj: any R object
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 6 Dec 1996

    is.fn <- func.names[substring(func.names,1,3) == "is."]
    is.fn <- is.fn[substring(is.fn,1,7) != "is.na<-"]
    use.fn <- is.fn[ is.na(match(is.fn, not.using))
                     & ! sapply(is.fn, is.method) ]

    r <- if(true.only) character(0)
    else structure(vector("list", length= length(use.fn)), names= use.fn)

    for(f in use.fn) {
        if(any(f == c("is.na", "is.finite"))) {
            if(!is.list(obj) && !is.vector(obj) && !is.array(obj)) {
                if(!true.only) r[[f]] <- NA
                next
            }
        }
        if(any(f == c("is.nan", "is.finite", "is.infinite"))) {
            if(!is.atomic(obj)) {
                if(!true.only) r[[f]] <- NA
                next
            }
        }
        if(debug) cat(f,"")
        fn <- get(f)
        rr <- if(is.primitive(fn) || length(formals(fn))>0)  fn(obj) else fn()
        if(!is.logical(rr)) cat("f=",f," --- rr	 is NOT logical	 = ",rr,"\n")

        ##if(1!=length(rr))   cat("f=",f," --- rr NOT of length 1; = ",rr,"\n")

        if(true.only && length(rr)==1 && !is.na(rr) && rr) r <- c(r, f)
        else if(!true.only) r[[f]] <- rr
    }
    if(debug)cat("\n")
    if(is.list(r)) structure(r, class = "isList") else r
}


# lists environments within a specified environment (DEFAULT = current)
# Author: cloudcello
# reference: http://blog.obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/
#' @export
ls_environments <- function(env=parent.frame())
{
    objs <- ls(all.names = TRUE, envir = env)
    # print(paste("number of objects", length(objs)))

    selLength=length(objs)

    # objClass <- vector(mode="character",length=selLength)
    selection <- vector(mode="logical",length=selLength)

    for (i in seq(selLength)) {
        # complexClass <- class(get(x=objs[i], envir = env))[1]
        # objClass[i] <- inherits(objs[i],what="environment")
        # print(class(get(x=objs[i], envir = env)))
        selection[i] <- "environment" %in% class(get(x=objs[i], envir = env))
        # objClass[i] <- class(get(x=objs[i], envir = env))[1]
    }
    # objs[which(objClass=="environment")]
    print(selection)
    print(which(selection==TRUE))
    objs[selection]

}




