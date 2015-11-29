################################################################################
# *** Borrowed Code ***
# Description: use this to try visualizing in Cytoscape
# Sourced from the following resources:
#          http://www.vesnam.com/Rblog/viznets1/
#          http://rcytoscape.systemsbiology.net/versions/current/index.html
################################################################################

################################################################################
# pre-install
# Code Source: http://rcytoscape.systemsbiology.net/versions/current/index.html
#
source ('http://bioconductor.org/biocLite.R')
biocLite ('RCytoscape')


library(BiocInstaller)
biocLite ('RCytoscape')

library (RCytoscape)
cy = CytoscapeConnection ()
pluginVersion (cy)

#Now, start R and type these commands:

library (RCytoscape)
cy = CytoscapeConnection ()
pluginVersion (cy)

# You should see
#
# [1] "1.8"
################################################################################


################################################################################
################################################################################
# Code Source: http://www.vesnam.com/Rblog/viznets1/

# Plotting networks in R
# An example how to plot networks and customize their appearance in Cytoscape directly from R, using RCytoscape package

############################################################################################
# Clear workspace
rm(list = ls())

# Load libraries
library("igraph")
library("plyr")

# Read a data set.
# Data format: dataframe with 3 variables; variables 1 & 2 correspond to interactions; variable 3 corresponds to the weight of interaction
dataSet <- read.table("lesmis.txt", header = FALSE, sep = "\t")

# Create a graph. Use simplify to ensure that there are no duplicated edges or self loops
gD <- simplify(graph.data.frame(dataSet, directed=FALSE))

# Print number of nodes and edges
# vcount(gD)
# ecount(gD)

############################################################################################
# Calculate some node properties and node similarities that will be used to illustrate
# different plotting abilities

# Calculate degree for all nodes
degAll <- degree(gD, v = V(gD), mode = "all")

# Calculate betweenness for all nodes
betAll <- betweenness(gD, v = V(gD), directed = FALSE) / (((vcount(gD) - 1) * (vcount(gD)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
rm(betAll)

#Calculate Dice similarities between all pairs of nodes
dsAll <- similarity.dice(gD, vids = V(gD), mode = "all")

############################################################################################
# Add new node/edge attributes based on the calculated node properties/similarities

gD <- set.vertex.attribute(gD, "degree", index = V(gD), value = degAll)
gD <- set.vertex.attribute(gD, "betweenness", index = V(gD), value = betAll.norm)

# Check the attributes
# summary(gD)

F1 <- function(x) {data.frame(V4 = dsAll[which(V(gD)$name == as.character(x$V1)), which(V(gD)$name == as.character(x$V2))])}
dataSet.ext <- ddply(dataSet, .variables=c("V1", "V2", "V3"), function(x) data.frame(F1(x)))

gD <- set.edge.attribute(gD, "weight", index = E(gD), value = 0)
gD <- set.edge.attribute(gD, "similarity", index = E(gD), value = 0)

# The order of interactions in gD is not the same as it is in dataSet or as it is in the edge list,
# and for that reason these values cannot be assigned directly

E(gD)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$weight <- as.numeric(dataSet.ext$V3)
E(gD)[as.character(dataSet.ext$V1) %--% as.character(dataSet.ext$V2)]$similarity <- as.numeric(dataSet.ext$V4)

# Check the attributes
# summary(gD)

####################################
# Print network in Cytoscape
# This requires RCytoscape package and CytoscapeRPC plugin

library("RCytoscape")

gD.cyt <- igraph.to.graphNEL(gD)

# We have to create attributes for graphNEL
# We'll keep the same name, so the values are passed from igraph

gD.cyt <- initNodeAttribute(gD.cyt, 'degree', 'numeric', 0)
gD.cyt <- initNodeAttribute(gD.cyt, 'betweenness', 'numeric', 0)
gD.cyt <- initEdgeAttribute (gD.cyt, "weight", 'integer', 0)
gD.cyt <- initEdgeAttribute (gD.cyt, "similarity", 'numeric', 0)

# Now we can create a new graph window in cytoscape
# Be sure that CytoscapeRPC plugin is activated
gDCW <- new.CytoscapeWindow("Les Miserables", graph = gD.cyt, overwriteWindow = TRUE)

# We can display graph, with defaults color/size scheme
displayGraph(gDCW)

# If you also want to choose a layout from R, a list  of available layouts can be accessed as follows:
cy <- CytoscapeConnection()
hlp <-getLayoutNames(cy)
# We'll select the layour number 18 - "fruchterman-rheingold" layout
# See properties for the given layout
# getLayoutPropertyNames(cy, hlp[18])
# Apply values to some of the properties and plot the layout
setLayoutProperties (gDCW, hlp[18], list (edge_attribute = 'similarity', iterations = 1000))
layoutNetwork(gDCW, hlp[18])

# Figure 1 made here

# Now, we can define our own default color/size scheme
setDefaultBackgroundColor(gDCW, '#FFFFFF')
setDefaultEdgeColor(gDCW, '#CDC9C9')
setDefaultEdgeLineWidth(gDCW, 4)
setDefaultNodeBorderColor(gDCW, '#000000')
setDefaultNodeBorderWidth(gDCW, 3)
setDefaultNodeShape(gDCW, 'ellipse')
setDefaultNodeColor(gDCW, '#87CEFA')
setDefaultNodeSize(gDCW, 60)
setDefaultNodeFontSize(gDCW, 20)
setDefaultNodeLabelColor(gDCW, '#000000')

# And we can replot it
redraw(gDCW)

# Figure 2 made here

# Now, we can replace some nodes with images
# You need to download images and put them in the "Images" folder
# Or you can change the code to use online images (provide URLs)
setNodeImageDirect (gDCW, c('Cosette', 'Fantine', 'Javert', 'JeanValjean'), c(sprintf ('file://%s/%s', getwd (), 'Images//Cosette.jpg'), sprintf ('file://%s/%s', getwd (), 'Images//Fantine.jpg'), sprintf ('file://%s/%s', getwd (), 'Images//Javert.jpg'), sprintf ('file://%s/%s', getwd (), 'Images//JeanValjean.jpg')))
redraw (gDCW)

# Figure 3 made here

# Finally, we can define rules for node colors, node sizes, and edge colors
setNodeColorRule(gDCW, 'degree', c(min(degAll), mean(degAll), max(degAll)), c('#F5DEB3', '#FFA500', '#FF7F50', '#FF4500', '#FF0000'), mode = 'interpolate')
setNodeSizeRule(gDCW, 'betweenness', c(min(betAll.norm), mean(betAll.norm), max(betAll.norm)), c(30, 45, 60, 80, 100), mode = 'interpolate')
setEdgeColorRule(gDCW, 'weight', c(min(as.numeric(dataSet.ext$V3)), mean(as.numeric(dataSet.ext$V3)), max(as.numeric(dataSet.ext$V3))), c('#FFFF00', '#00FFFF', '#00FF7F', '#228B22', '#006400'), mode='interpolate')
redraw (gDCW)

# Figure 4 made here
#
#
#

