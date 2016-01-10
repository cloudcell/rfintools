
# Base Code Source: QuantStrat package
# Extensions: cloudcell
# Date: January 2016
#
# Warning: this code is still under heavy development
#          most of it is experimental
#
# Note: modifications:
#       1) fixed the aspect ratio to rgl::persp3d()
#          charts are distorted otherwise (if they have unequal # of params in each dimension )
#       2) chart rainbow color scheme: some code borrowed from StkOvflow
#          source: http://stackoverflow.com/questions/17258787/formating-of-persp3d-plot
#
# ---------------------------------------------------------------------------- -
# Selected reference:
#     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   * Viridis seems to be most scientifically based (better than perula)
#     Nathaniel Smith and Stéfan van der Walt presented a new colormap (for Python) at SciPy 2015 called viridis
#
#     (watch the video as well)
#     http://rud.is/b/2015/07/20/using-the-new-viridis-colormap-in-r-thanks-to-simon-garnier/
#     - install python code from the video & build your own colormap
#
#     http://bids.github.io/colormap/
#
#     https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
#
#     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   * on visualizing data properly:
#     http://www.research.ibm.com/people/l/lloydt/color/color.HTM
#   * on reasons for NOT using the rainbow color scheme
#     - http://geog.uoregon.edu/datagraphics/EOS/Light-and-Bartlein_EOS2004.pdf
#     - https://eagereyes.org/basics/rainbow-color-map
#     - (also, the mid-range hues of the palette become indistinguishable)
#   * Escaping RGBland: Selecting Colors for Statistical Graphics
#     http://statmath.wu.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf
#   * Choosing colour palettes. Part II: Educated Choices
#     http://www.r-bloggers.com/choosing-colour-palettes-part-ii-educated-choices/
#   * Choosing The Color Palette, Part III:
#     http://piktochart.com/blog/choosing-the-color-palette-part-iii-the-rule-of-3-colors/
#   * a good demo of various colorschemes
#     http://sudillap.hatenablog.com/entry/2013/05/07/213052
#     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
#   * List of color spaces and their uses
#     https://en.wikipedia.org/wiki/List_of_color_spaces_and_their_uses
#
#     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#     CIE is not perfect !!!
#     CIE 1931 XYZ -- The first attempt to produce a color space based on
#     measurements of human color perception and it is the basis for almost
#     all other color spaces.
#
#     CIELUV -- A modification of "CIE 1931 XYZ" to display color differences
#     more conveniently. The CIELUV space is especially useful for
#     additive mixtures of lights, due to its linear addition properties.
#
#   * CIELUV colorspace is used in MagnaView / PaletteView can be found in
#     this package
#     https://cran.r-project.org/web/packages/colorspace/colorspace.pdf
#     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   * parula (new matlab palette)
#     (has inherent defects compared to viridis, see above under 'viridis')
#     https://github.com/Gnuplotting/gnuplot-palettes/blob/master/parula.pal
#     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
# FIXME: aspect ratio must still be adjusted ----

# aspect
# either a logical indicating whether to adjust the aspect ratio, or a new ratio.
# SOLUTION:
#   adjust x/y ratio based on the ratio of x-range to y-range
#   keep z always equal to 1.00 (or 100)

# TODO: make the code return an object for printing.
#       print within the function is not used to make the function compatible
#       with knittr
#
# colsch - color scheme
tradeGraphs_asp <- function(stats,
                            free.params,
                            params.filter = NULL,
                            statistics,
                            title = NULL,
                            print_gr=FALSE, # for compatibility with knitr, set to false (==default)
                            debug=FALSE, # debug
                            colsch=c('viridis','heat','rbow','rbowStripesD1','rbowStripesD2'), # colorscheme
                            contour=FALSE,
                            zcuts=30,
                            type=c('lp','levelplot', # levelplot
                                   '3d','rgl',       # type: 3d
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

    if(missing(stats))
        stop('stats undefined')

    if(missing(free.params))
        stop('free.params undefined')
    if(length(free.params) != 2)    stop('free.params must be a vector of length 2')

    if(missing(statistics))
        stop('must specify at least one statistics column to draw graph')

    # var1 <- free.params[1]
    # var2 <- free.params[2]
    var1 <- format.default(free.params[1],scientific = FALSE) # FIXME this is a param. NAME !!!
    var2 <- format.default(free.params[2],scientific = FALSE) # same as above !


    # the following is used for knitr compatibility
    # accumulate graphs to be printed externally, if needed
    graphs.lst <- vector(length = length(statistics), mode = "list")

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
               # NAs are kept as is
               # colors == "ranked palette" (each palette unit is used once !)
               'heat' = {
                   palette = heat.colors(nonNALength(z))
                   colors <- palette[rank(z, na.last="keep")]
               },
               'terr' = {
                   palette = terrain.colors(nonNALength(z))
                   colors <- palette[rank(z, na.last="keep")]
               },
               'rbow' = {
                   nbcol = nonNALength(z)
                   palette = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                   colors <- palette[rank(z, na.last="keep")]
               },
               'rbow_rgb' = {
                   nbcol = nonNALength(z)
                   palette = rainbow(nbcol, start = 0/6, end = 4/6)
                   colors <- palette[rank(z, na.last="keep")]
               },
               'matlab' = {
                   nbcol = nonNALength(z)
                   # install.packages("colorRamps")
                   require(colorRamps)
                   palette = matlab.like(nbcol) # TODO: consider using a smaller number of colors for levelplot() ----
                   colors <- palette[rank(z, na.last="keep")]
               },
               'matlab2' = {
                   nbcol = nonNALength(z)
                   # install.packages("colorRamps")
                   require(colorRamps)
                   palette = matlab.like2(nbcol) # TODO: consider using a smaller number of colors for levelplot() ----
                   colors <- palette[rank(z, na.last="keep")]
               },
               'RdYlBu' = {
                   nbcol = nonNALength(z) # TODO: consider using a smaller number of colors for levelplot() ----
                   # install.packages("RColorBrewer")
                   require(RColorBrewer)
                   palette =   rev(brewer.pal(n=11,name = 'RdYlBu')) # 11 - max! :(
                   colors <- palette[rank(z, na.last="keep")]
               },
               'RdBu' = {
                   nbcol = nonNALength(z) # TODO: consider using a smaller number of colors for levelplot() ----
                   # install.packages("RColorBrewer")
                   require(RColorBrewer)
                   palette =   rev(brewer.pal(n=11,name = 'RdBu')) # 11- max !
                   colors <- palette[rank(z, na.last="keep")]
               },
               'BrBG' = {
                   nbcol = nonNALength(z) # TODO: consider using a smaller number of colors for levelplot() ----
                   # install.packages("RColorBrewer")
                   require(RColorBrewer)
                   palette =   rev(brewer.pal(n=11,name = 'BrBG')) # 11- max !
                   colors <- palette[rank(z, na.last="keep")]
               },
               'RdYlGn' = {
                   nbcol = nonNALength(z) # TODO: consider using a smaller number of colors for levelplot() ----
                   # install.packages("RColorBrewer")
                   require(RColorBrewer)
                   palette =   rev(brewer.pal(n=11,name = 'RdYlGn')) # 11- max !
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
               'byrb' = {
                   require(grDevices) # for colorRampPalette

                   nbcol = nonNALength(z) # non-NA-length is validated for
                                          # cases where rank() is used

                   # assign this magic number to a variable equal to color key ('cuts' argument in levelplot)
                   # palette=colorRampPalette(c("blue", "yellow","red", "black"))(15+1) # 'chunks' = 'cuts' + 1
                   palette=colorRampPalette(c("blue", "yellow","red", "black"))(zcuts) # 'chunks' = 'cuts' + 1
                   # palette = rev(rainbow(nbcol, start = 0/6, end = 4/6))
                   # Breaks the Factor w/ 100 levels "(-2.18,-2.05]",..: 25 24 23 20 17 15 13 12 11 11 ...
                   zcol  = cut(z, nbcol)
                   colors=palette[zcol]
               },
               'viridis' = {
                   # install.packages("viridis")
                   require(viridis)

                   nbcol = nonNALength(z) # non-NA-length is validated for
                                          # cases where rank() is used


                   # assign this magic number to a variable equal to color key ('cuts' argument in levelplot)
                   palette=viridis_pal()(15+1) # 'chunks' = 'cuts' + 1
                   palette=viridis_pal()(nbcol) # 'chunks' = 'cuts' + 1
                   # FIXME: ----
                   palette=viridis_pal()(zcuts) # 'chunks' = 'cuts' + 1

                   # palette=colorRampPalette(c("blue", "yellow","red", "black"))(15+1) # 'chunks' = 'cuts' + 1
                   # palette = rev(rainbow(nbcol, start = 0/6, end = 4/6))
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

        cat("data prepared\n")

        main_title=paste0(var3," {Study: \"",title,"\"}")

        switch(type[1],
               '3d'=,
               'rgl'={
                   cat("opening a 3D device\n")
                   # note: sometimes this call produces an error:
                   # Error in t.default(La.res$vt) : argument is not a matrix
                   # caused by "rgl::par3d(params)" (inside the call)
                   rgl::open3d()

                   # rgl::aspect3d(x=103, y=84, z =0.5)
                   cat("drawing a 3D chart\n")
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
                   # dev.new()
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

                   level_qty=min(length(palette),15+2+2) # 15 'cuts'within +2 = on both sides / or +1 for midsections : FIXME ----

                   graphs.lst[[var3]] <-
                       levelplot(x=z,
                                        col.regions=palette,
                                        # col.regions=colorRampPalette(c("blue", "yellow","red", "black")),
                                        # colorkey=list(tick.number=level_qty,
                                        colorkey=list(tick.number=zcuts,
                                                      labels=list(cex=0.5)),
                                        # cuts=level_qty-1, # The number of cuts (levels) the range of z would be divided by(into).
                                        # FIXME ----
                                        cuts=zcuts-1,
                                        # region=TRUE,
                                        # col.regions=heat.colors(1000),
                                        main.cex=0.5,
                                        contour=contour,
                                        # labels=TRUE,
                                        scales=list(cex=0.5,
                                                    tck=0.5,
                                                    # y=list(tick.number=25),
                                                    #
                                                    x=list(rot=90,labels=x.labels, at=x.at),
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
                   if(print_gr) print(graphs.lst[var3])
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

    return(graphs.lst)
}



# TODO: # create plottable data via tradeGraphData() function
# factor out that part of the code

############################################################################## #
# code to delete:


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



