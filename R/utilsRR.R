


# returns a reference to robustR environment if it is present / or was created,
# fails otherwise
robustRenv.must.exist <- function(env=globalenv())
{
    if(exists(x = ".robustR.env", envir = env)) {
        if(!is.environment(env$.robustR.env)) {
            stop("Conflict: .robustR.env is present but is not an environment")
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
checkRobustR.env() <- function(env=globalenv())
{
    if(!exists(".robustR.env", envir = env)) stop(".robustR.env missing!")
    get(x = ".robustR.env",envir = env)
}


