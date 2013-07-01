# Pick the folder in which you have saved this file and plan to save the outputs
setwd("~/Dropbox/Pegasus/2013_06_28_dhch_workshops/dhch-network")

# If the command below doesn't work, do 
# install.packages("igraph") and then library(igraph) again
library(igraph)

# Reading the file of the first workshop session
w1 <- readLines("data/workshops1")

# Separating the 4 workshops
w1a <- w1[1:23]
w1b <- w1[25:42]
w1c <- w1[44:71]
w1d <- w1[73:89]

# Same with second session
w2 <- readLines("data/workshops2")

w2a <- w2[1:26]
w2b <- w2[28:54]
w2c <- w2[56:76]
w2d <- w2[78:89]

# Same with third session
w3 <- readLines("data/workshops3")

w3a <- w3[1:36]
w3b <- w3[38:54]
w3c <- w3[56:68]
w3d <- w3[70:87]

# We create a list with participats to all 12 twelve workshops
wlol <- list(Pierazzo = w1a[-1], 
             Siemens = w1b[-1], 
             Berry = w1c[-1], 
             Chacherau = w1d[-1], 
             Lemercier = w2a[-1], 
             Schreibman = w2b[-1], 
             Clivaz = w2c[-1], 
             Pugin = w2d[-1], 
             Kaplan = w3a[-1], 
             Fohr = w3b[-1], 
             Koller = w3c[-1], 
             Grandjean = w3d[-1])

# Let's suppress tabulations ("\t") in their names
wlol <- lapply(wlol, function(x) gsub("\t", "", x))

# This time let's suppress backslashes
wlol <- lapply(wlol, function(x) gsub("\\\\", "", x))

# [debug] in order to control that there are no doubles
# sort(unique(as.vector(unlist(wlol))))

# We prepare an empty list for edge list
edge.list <- list()

# This loop will associate workshops and participants
# line after line
for (i in 1:length(wlol)) edge.list[[i]] <- cbind(names(wlol)[i], wlol[[i]])

# And we transform the edge list to a data frame, so that igraph can read it
edges <- do.call(rbind, edge.list)

# Then we ask igraph to create the graph
g <- graph.data.frame(edges, directed = FALSE)

# Then the projection (bipartite.mapping finds the bipartite partition)
h <- bipartite.projection(g, types = bipartite.mapping(g)$type)$proj2

# For example, so that it is readable, we can keep ties between people with two or more workshops in common 
h2 <- subgraph.edges(h, eids = E(h)[E(h)$weight >= 2], delete.vertices = FALSE)


###############################################
### The plot of the projection network (h2) ###
###############################################

# These are graphical parameters for vertices
V(h2)$label.color <- "black"
V(h2)$label.cex <- 1.5
V(h2)$size <- 5
V(h2)$color <- "orange"

# Graphical parameters for edges
E(h2)$color <- "black"

# Layout (position of nodes)
h2$layout <- layout.fruchterman.reingold(h2, repulserad = vcount(h2)^2.5)

# We suppress the margins, useful for stat graphs but not here
par(mar=c(0,0,0,0))

# This plots the graph
plot(h2)


##############################################
### The plot of the projection network (g) ###
##############################################

# Let's begin by building a layout with special position for the 12 workshops
layout.temp <- layout.circle(graph.empty(12))[sample(1:12),]

# We put all the nodes on one point in the middle
g$layout <- matrix(0, ncol = 2, nrow = vcount(g))

# Then we give their positions to the 12 workshops
g$layout[1:12,] <- layout.temp

# And we let all them find the position that would suit them best
g$layout <- layout.fruchterman.reingold(g, start = g$layout, niter = 5000)

# Graphical parameters
V(g)$label.color <- "black"
V(g)$label.cex <- 1.5
V(g)$size <- 5
V(g)$color <- "orange"

# Workshops will appear in red
V(g)[1:12]$color <- "red1"

# Margins etc.
par(mar=c(0,0,0,0))

# Plotting etc.
plot(g)

# In this case or the previous one, the output may be deformed. 
# One solution is to produce a png with size 2000x2000
# See ?png in the console