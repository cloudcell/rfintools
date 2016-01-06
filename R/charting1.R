
# Code Source: QuantStrat package
#
# Note: modifications:
#       1) fixed the aspect ratio to rgl::persp3d()
#          charts are distorted otherwise (if they have unequal # of params in each dimension )
#       2) chart rainbow color scheme: some code borrowed from StkOvflow
#          source: http://stackoverflow.com/questions/17258787/formating-of-persp3d-plot
#
# Reference:
#   * on visualizing data properly:
#     http://www.research.ibm.com/people/l/lloydt/color/color.HTM
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
                            debug=FALSE, # debug
                            colsch=c('heat','rbow','rbowStripesD1','rbowStripesD2'), # colorscheme
                            type=c('3d','rgl',       # type: 3d
                                   'lp','levelplot', # levelplot
                                   'hm','heatmap'))  # heatmap
{
    ._fn = "tradeGraphs_asp():" # func. name
    cat(._fn, "function entry\n")

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

        # rank assigns a rank to NA's !
        # try a <- c(NA,NA,NA,1,2,3)
        # rank(a)
        # rank(a,na.last="keep")
        # [1] 4 5 6 1 2 3
        # this issue must be dealt with

        # deducts the number of NAs in the vector
        nonNALength <- function(v)
        {
            length(v[!is.na(v)])
        }

        switch(colsch[1],
               # ranked palette (each palette unit is used once !)
               'heat' = {
                   # every color is used once, NA's are kept as is
                   colors <- heat.colors(nonNALength(z))[rank(z, na.last="keep")]
                   # colors <- heat.colors(length(z))[rank(z)]
                   # colors <- terrain.colors(nonNALength(z))[rank(z, na.last="keep")]
               },
               # ranked palette (each palette unit is used once !)
               'rbow' = {
                   nbcol = nonNALength(z)
                   palette = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                   # every color is used once, NA's are kept as is
                   colors <- palette[rank(z, na.last="keep")]
               },
               # ranked palette (each palette unit is used once !)
               'rbow_rgb' = { # palette as is -- not reversed
                   nbcol = nonNALength(z)
                   palette = rainbow(nbcol, start = 0/6, end = 4/6)
                   # every color is used once, NA's are kept as is
                   colors <- palette[rank(z, na.last="keep")]
               },
               # colored by value (each palette unit may be used MORE than once !)
               'rbow2' = {
                   nbcol = nonNALength(z) # non-NA-length is validated for
                                          # cases where rank() is used
                   palette = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                   # Breaks the Factor w/ 100 levels "(-2.18,-2.05]",..: 25 24 23 20 17 15 13 12 11 11 ...
                   zcol  = cut(z, nbcol)
                   colors=palette[zcol]
               },
               # D=MARGIN a vector giving the subscripts which the function will
               # be applied over. E.g., for a matrix 1 indicates rows, 2
               # indicates columns, c(1, 2) indicates rows and columns. Where X
               # has named dimnames, it can be a character vector selecting
               # dimension names.
               # see apply function, argument MARGIN (2nd arg.)
               # -= Colored "Y" slices =-
               'rbowStripesD1' = {
                   # a crude hack
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
               },
               stop("invalid color scheme specified")
        )


        if(debug) browser()

        main_title=paste0(var3," {Study: \"",title,"\"}")

        switch(type[1],
               '3d'=,
               'rgl'={
                   # heatmap(hex2num colors)
                   rgl::open3d()
                   # rgl::aspect3d(x=103, y=84, z =0.5)
                   rgl::persp3d(x,y,z,
                                color=colors,
                                xlab=var1,
                                ylab=var2,
                                zlab=var3,
                                main=main_title,
                                # aspect=c(x=104, y=85, z =100))#, aspect=FALSE)
                                aspect=c(x=100, y=length(y)/length(x)*100, z =100),
                                top=TRUE)#, aspect=FALSE)
               },
               'lp'=,
               'levelplot'={
                   require(lattice)
                   z=matrix(z,nrow = length(x),ncol = length(y), byrow = FALSE)
                   dev.new()
                   # require(grDevices) # for colorRampPalette
                   # par(oma=c(0,0,0,0))
                   cat("almost done\n")


                   # set the num. of ticks
                   x.tick.number <- 15
                   y.tick.number <- 15
                   # which labels to print
                   x.at <- round( seq(1, max(x)-min(x)+1, by=5))#length.out=x.tick.number) ) # TODO: use pretty
                   y.at <- round( seq(1, max(y)-min(y)+1, by=5))#length.out=y.tick.number) )
                   # x.at <- round( seq(1, max(x)-min(x)+1, length.out=x.tick.number) ) # TODO: use pretty
                   # y.at <- round( seq(1, max(y)-min(y)+1, length.out=y.tick.number) )
                   # labels to print
                   x.labels <- as.character( round(seq(min(x),max(x), by=5)))#length.out=x.tick.number)) )
                   y.labels <- as.character( round(seq(min(y),max(y), by=5)))#length.out=y.tick.number)) )
                   # x.labels <- as.character( round(seq(min(x),max(x), length.out=x.tick.number)) )
                   # y.labels <- as.character( round(seq(min(y),max(y), length.out=y.tick.number)) )


                   plot_lp <- levelplot(x=z,
                                        col.regions=palette,
                                        # col.regions=colorRampPalette(c("blue", "yellow","red", "black")),
                                        colorkey=list(tick.number=10,  labels=list(cex=0.5)),
                                        cuts=15,
                                        # region=TRUE,
                                        # col.regions=heat.colors(1000),
                                        main.cex=0.5,
                                        contour=TRUE,
                                        # labels=TRUE,
                                        scales=list(cex=0.5,
                                                    tck=0.5,
                                                    # y=list(tick.number=25),
                                                    #
                                                    x=list(rot=0,labels=x.labels, at=x.at),
                                                    y=list(rot=0, labels=y.labels, at=y.at)
                                                    #
                                        ), # FIXME: see ?xyplot -- xlim for character vectors
                                        # xlim=pretty(x,n=10),
                                        # ylim=pretty(y,n=10),
                                        # xlim=c(min(x),max(x)),
                                        # ylim=c(min(y),max(y)),
                                        # xlim=as.character(x),
                                        # ylim=as.character(y),
                                        # ylim=y,

                                        main=list(label=main_title, cex=0.8),
                                        xlab=list(label=var1, cex=.5),
                                        ylab=list(label=var2, cex=.5) #,
                                        # at=seq(min(z,na.rm = TRUE),max(z,na.rm = TRUE),length.out = 30)
                   )
                   print(plot_lp)
                   # contourplot(z,
                   #           # col.regions=palette,
                   #           col.regions=heat.colors(1000),
                   #           xlab=var1,
                   #           ylab=var2, main.cex=0.5)
                   #
                   # vectorplot(as.data.frame (z))
               },
               'hm'=,
               'heatmap'={
                   require(gplots)
                   z=matrix(z,nrow = length(x),ncol = length(y), byrow = FALSE)
                   dev.new()
                   heatmap.2(x=z, Rowv=NULL,Colv=NULL,
                             # heatmap.2(x=matrix(rnorm(20*10), nrow=10), Rowv=NULL,Colv=NULL,
                             col = palette,
                             # col = rev(rainbow(20*10, start = 0/6, end = 4/6)),
                             scale="none",
                             margins=c(3,0), # ("margin.Y", "margin.X")
                             trace='none',
                             # trace='both',
                             hline = 100,
                             symbreaks=FALSE,
                             dendrogram='none',
                             density.info='histogram',
                             denscol="black",
                             symkey=FALSE,
                             key.title="",
                             key.xlab=NA,
                             keysize=1, # seems not to matter if larger than margins allowed by key.par
                             # key parameters ("par()")
                             key.par=list(mar=c(3,0,1,0),  #( "bottom.padding", "left.padding", "top.padding", "left.padding" )
                                          cex=0.5,
                                          font=par("font.axis")),
                             # lmat -- added 2 lattice sections (5 and 6) for padding
                             lmat=rbind(c(7,8,9),c(5, 4, 2), c(6, 1, 3)), lhei=c(0.1, 1, 12), lwid=c(0.6, 12, 0.6)#,
                             #title & labels
                             # main=title,
                             # sub=title
                   )
                   title(main=main_title)
               },
               stop("invalid graph type specified")
        )
    } # end 'for'

    cat(._fn,"function exit\n")
}



# TODO: # create plottable data via tradeGraphData() function
# factor out that part of the code

############################################################################## #
# code to delete:
tradeGraphs_hm<- function(stats,
                          free.params,
                          params.filter = NULL,
                          statistics,
                          title = NULL,
                          colsch=c('heat','rbow','rbowStripesD1','rbowStripesD2'),
                          debug=FALSE) # colorscheme
{
    ._fn = "tradeGraphData():" # func. name
    cat(._fn, "function entry\n")

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

        # rank assigns a rank to NA's !
        # try a <- c(NA,NA,NA,1,2,3)
        # rank(a)
        # rank(a,na.last="keep")
        # [1] 4 5 6 1 2 3
        # this issue must be dealt with

        # deducts the number of NAs in the vector
        nonNALength <- function(v)
        {
            length(v[!is.na(v)])
        }

        found <- FALSE
        switch(colsch[1],
               # ranked palette (each palette unit is used once !)
               'heat' = {
                   # every color is used once, NA's are kept as is
                   colors <- heat.colors(nonNALength(z))[rank(z, na.last="keep")]
                   # colors <- heat.colors(length(z))[rank(z)]
                   # colors <- terrain.colors(nonNALength(z))[rank(z, na.last="keep")]
                   found <- TRUE
               },
               # ranked palette (each palette unit is used once !)
               'rbow' = {
                   nbcol = nonNALength(z)
                   palette = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                   # every color is used once, NA's are kept as is
                   colors <- palette[rank(z, na.last="keep")]
                   found <- TRUE
               },
               # ranked palette (each palette unit is used once !)
               'rbow_rgb' = { # palette as is -- not reversed
                   nbcol = nonNALength(z)
                   palette = rainbow(nbcol, start = 0/6, end = 4/6)
                   # every color is used once, NA's are kept as is
                   colors <- palette[rank(z, na.last="keep")]
                   found <- TRUE
               },
               # colored by value (each palette unit may be used MORE than once !)
               'rbow2' = {
                   nbcol = nonNALength(z) # non-NA-length is validated for
                   # cases where rank() is used
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
                   # a crude hack
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

        # rgl::open3d()
        # rgl::persp3d(x,y,z, color=colors, xlab=var1, ylab=var2, zlab=var3, main = title,
        #              # aspect=c(x=104, y=85, z =100))#, aspect=FALSE)
        #              aspect=c(x=100, y=length(y)/length(x)*100, z =100),
        #              top=TRUE)#, aspect=FALSE)
        #
        # as.matrix()
        # str(z)
        # heatmap(x=z, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
        # require(gplots)
        # heatmap.2(x=z, Rowv=NA, Colv=NA, col = palette, scale="none", margins=c(5,10))

        if(debug) browser()

        z=matrix(z,nrow = length(x),ncol = length(y), byrow = FALSE)
        dev.new()
        # dev.off()
        # title(main=title, line = 0)
        if(0) {
            require(gplots)
            heatmap.2(x=z, Rowv=NULL,Colv=NULL,
                      # heatmap.2(x=matrix(rnorm(20*10), nrow=10), Rowv=NULL,Colv=NULL,
                      col = palette,
                      # col = rev(rainbow(20*10, start = 0/6, end = 4/6)),
                      scale="none",
                      margins=c(3,0), # ("margin.Y", "margin.X")
                      trace='none',
                      # trace='both',
                      hline = 100,
                      symbreaks=FALSE,
                      dendrogram='none',
                      density.info='histogram',
                      denscol="black",
                      symkey=FALSE,
                      key.title="",
                      key.xlab=NA,
                      keysize=1, # seems not to matter if larger than margins allowed by key.par
                      # key parameters ("par()")
                      key.par=list(mar=c(3,0,1,0),  #( "bottom.padding", "left.padding", "top.padding", "left.padding" )
                                   cex=0.5,
                                   font=par("font.axis")),
                      # lmat -- added 2 lattice sections (5 and 6) for padding
                      lmat=rbind(c(7,8,9),c(5, 4, 2), c(6, 1, 3)), lhei=c(0.1, 1, 12), lwid=c(0.6, 12, 0.6)#,
                      #title & labels
                      # main=title,
                      # sub=title
            )
        }

        require(lattice)
        # require(grDevices) # for colorRampPalette
        # par(oma=c(0,0,0,0))
        levelplot(x=z,
                  col.regions=palette,
                  # col.regions=colorRampPalette(c("blue", "yellow","red", "black")),
                  colorkey=list(tick.number=10,  labels=list(cex=0.5)),
                  cuts=15,
                  # region=TRUE,
                  # col.regions=heat.colors(1000),
                  main.cex=0.5,
                  contour=TRUE,
                  # labels=TRUE,
                  scales=list(cex=0.5, tck=0.5, tick.number=25),
                  main=list(label=title, cex=0.8),
                  xlab=list(label=var1, cex=.5),
                  ylab=list(label=var2, cex=.5) #,
                  # at=seq(min(z,na.rm = TRUE),max(z,na.rm = TRUE),length.out = 30)
                  )

        # contourplot(z,
        #           # col.regions=palette,
        #           col.regions=heat.colors(1000),
        #           xlab=var1,
        #           ylab=var2, main.cex=0.5)
        #
        # vectorplot(as.data.frame (z))
        # title(main=title,
        #       xlab=var1,
        #       ylab=var2,
        #       outer=TRUE) #, outer=TRUE)#, line = 0)
        # title(sub=title)#, outer=TRUE)#, line = 0)

    } # end 'for'

    cat(._fn,"function exit\n")
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



