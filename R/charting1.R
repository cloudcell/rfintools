
# Code Source: QuantStrat package
#
# Note: I simply fixed the aspect ratio to rgl::persp3d()
#       charts are distorted otherwise (if they have unequal # of params in each dimension )
#
# FIXME: aspect ratio must still be adjusted ----

# aspect
# either a logical indicating whether to adjust the aspect ratio, or a new ratio.
# SOLUTION:
#   adjust x/y ratio based on the ratio of x-range to y-range
#   keep z always equal to 1.00 (or 100)


tradeGraphs_asp <- function(stats,
                            free.params,
                            params.filter = NULL,
                            statistics,
                            title = NULL)
{
    # TODO: fix axes to use non scientific notation ----
    # TODO: fix use of full rainbow for small fractions (eg. Profit.Factor, now only uses red) ----

    if(!requireNamespace("rgl", quietly=TRUE))
        stop('The "rgl" package is required to use this function')

    if(!requireNamespace("reshape2", quietly=TRUE))
        stop('The "reshape2" package is required to use this function')

    if(missing(stats))      stop('stats undefined')

    if(missing(free.params))        stop('free.params undefined')
    if(length(free.params) != 2)    stop('free.params must be a vector of length 2')

    if(missing(statistics))         stop('must specify at least one statistics column to draw graph')

    # var1 <- free.params[1]
    # var2 <- free.params[2]
    var1 <- format.default(free.params[1],scientific = FALSE)
    var2 <- format.default(free.params[2],scientific = FALSE)

    for(var3 in statistics) {
        if (length(params.filter) == 0 ) {
            data <- stats[,c(var1, var2, var3)]
        } else {
            data <- subset(stats, eval(parse(text=params.filter)), select =  c(var1, var2, var3))
        }

        data_r <- reshape2::recast(data, as.formula(paste0(var1, " ~ ", var2)),
                                   id.var=c(var1,var2), measure.var=c(var3))
        x <- data_r[, 1]
        y <- as.numeric(colnames(data_r)[-1])
        z <- unlist(data_r[, -1])

        col <- heat.colors(length(z))[rank(z)]
        # col <- terrain.colors(length(z))[rank(z)]

        rgl::open3d()
        # rgl::aspect3d(x=103, y=84, z =0.5)
        rgl::persp3d(x,y,z, color=col, xlab=var1, ylab=var2, zlab=var3, main = title,
                     # aspect=c(x=104, y=85, z =100))#, aspect=FALSE)
                     aspect=c(x=100, y=length(y)/length(x)*100, z =100))#, aspect=FALSE)
    }
}

if(0){
    # http://www.inside-r.org/packages/cran/rgl/docs/aspect3d
    rgl::par3d("scale")
    aspect3d("iso")

    aspect3d(x, y = NULL, z = NULL)

    rgl::aspect3d(x=100, y=100, z =0.57)

    # Arguments
    # x 	The ratio for the x axis, or all three ratios, or "iso"
    # y 	The ratio for the y axis
    # z 	The ratio for the z axis
}



