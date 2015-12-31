
# Code Source: QuantStrat package
#
# Note: modifications:
#       1) fixed the aspect ratio to rgl::persp3d()
#          charts are distorted otherwise (if they have unequal # of params in each dimension )
#       2) chart rainbow color scheme: some code borrowed from StkOvflow
#          source: http://stackoverflow.com/questions/17258787/formating-of-persp3d-plot
#
#
# FIXME: aspect ratio must still be adjusted ----

# aspect
# either a logical indicating whether to adjust the aspect ratio, or a new ratio.
# SOLUTION:
#   adjust x/y ratio based on the ratio of x-range to y-range
#   keep z always equal to 1.00 (or 100)

# colsch - color scheme
tradeGraphs_asp <- function(stats,
                            free.params,
                            params.filter = NULL,
                            statistics,
                            title = NULL,
                            colsch=c('heat','rbow','rbowStripesD1','rbowStripesD2')) # colorscheme
{
    if(0) browser()
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
    var1 <- format.default(free.params[1],scientific = FALSE) # FIXME this is a param. NAME !!!
    var2 <- format.default(free.params[2],scientific = FALSE) # same as above !

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

        # the 'names' of z are spliced x&y parameter values used as names
        # the default ordering is by y(min:max){ x(min:max) }
        z <- unlist(data_r[, -1])

        # if(colsch[1]=='heat') {
        #
        # } else {
        #     if(colsch[1]=='rbow')
        # else {
        #     if( )
        #     stop("invalid color scheme specified")}
        # browser()
        found <- FALSE
        switch(colsch[1],
               'heat' = {
                   colors <- heat.colors(length(z))[rank(z)]
                   # col <- terrain.colors(length(z))[rank(z)]
                   found <- TRUE
               },
               # ranked palette (each palette unit is used once !)
               'rbow' = {

                   nbcol = length(z)
                   palette = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                   # Breaks the Factor w/ 100 levels "(-2.18,-2.05]",..: 25 24 23 20 17 15 13 12 11 11 ...
                   zcol  = cut(z, nbcol)
                   if(0) colors=palette[zcol]

                   # colors <- heat.colors(length(z))[rank(z)]
                   colors <- palette[rank(z)] # every color is used one time !
                   found <- TRUE
               },
               # colored by value  (each palette may be used more than once !)
               'rbow2' = {

                   nbcol = length(z)
                   palette = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                   # Breaks the Factor w/ 100 levels "(-2.18,-2.05]",..: 25 24 23 20 17 15 13 12 11 11 ...
                   zcol  = cut(z, nbcol)
                   colors=palette[zcol]

                   found <- TRUE
               },
               # D=MARGIN a vector giving the subscripts which the function will
               # be applied over. E.g., for a matrix 1 indicates rows, 2
               # indicates columns, c(1, 2) indicates rows and columns. Where X
               # has named dimnames, it can be a character vector selecting
               # dimension names.
               # see apply function, argument MARGIN (2nd arg.)
               # -= Colored "Y" slices =-
               'rbowStripesD1' = {
                   # this is a hack
                   if(1){
                       if (length(params.filter) == 0 ) {
                           data <- stats[,c(var2, var1, var3)]
                       } else {
                           data <- subset(stats, eval(parse(text=params.filter)), select =  c(var2, var1, var3))
                       }
                       data_r <- reshape2::recast(data, as.formula(paste0(var2, " ~ ", var1)),
                                                  id.var=c(var2,var1), measure.var=c(var3))
                       x <- data_r[, 1]
                       y <- as.numeric(colnames(data_r)[-1])
                       z <- unlist(data_r[, -1])
                   }

                   nbcol = length(x)
                   color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                   mycut = function(x, breaks) as.numeric(cut(x=x, breaks=breaks)) # TODO rewrite this more clearly ! ----

                   z=matrix(z,nrow = length(x),ncol = length(y), byrow = FALSE)

                   zcol2 = as.numeric(apply(X=z,2, mycut, breaks=length(x)))
                   colors=color[zcol2]
                   found <- TRUE
                   tmp <- var1
                   var1 <- var2
                   var2 <- tmp
               },
               # -= Colored "X" slices =-
               'rbowStripesD2' = {
                   nbcol = length(x)
                   color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                   mycut = function(x, breaks) as.numeric(cut(x=x, breaks=breaks)) # TODO rewrite this more clearly ! ----

                   z=matrix(z,nrow = length(x),ncol = length(y), byrow = FALSE)

                   zcol2 = as.numeric(apply(X=z,2, mycut, breaks=length(x)))
                   colors=color[zcol2]
                   found <- TRUE
               }
        )
        if(!found) stop("invalid color scheme specified")
        # heatmap(hex2num colors)
        rgl::open3d()
        # rgl::aspect3d(x=103, y=84, z =0.5)
        rgl::persp3d(x,y,z, color=colors, xlab=var1, ylab=var2, zlab=var3, main = title,
                     # aspect=c(x=104, y=85, z =100))#, aspect=FALSE)
                     aspect=c(x=100, y=length(y)/length(x)*100, z =100),
                     top=TRUE)#, aspect=FALSE)
    } # end 'for'

}


# < sandbox > ----
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



