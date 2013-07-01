setwd("~/Dropbox/Pegasus/2013_06_28_dhch_workshops/")

library(igraph)

w1 <- readLines("workshops1")

w1a <- w1[1:23]
w1b <- w1[25:42]
w1c <- w1[44:71]
w1d <- w1[73:89]

w2 <- readLines("workshops2")

w2a <- w2[1:26]
w2b <- w2[28:54]
w2c <- w2[56:76]
w2d <- w2[78:89]

w3 <- readLines("workshops3")

w3a <- w3[1:36]
w3b <- w3[38:54]
w3c <- w3[56:68]
w3d <- w3[70:87]

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

wlol <- lapply(wlol, function(x) gsub("\t", "", x))
wlol <- lapply(wlol, function(x) gsub("\\\\", "", x))

# in order to control that there are no doubles
# sort(unique(as.vector(unlist(wlol))))

edge.list <- list()
for (i in 1:length(wlol)){
edge.list[[i]] <- cbind(names(wlol)[i], wlol[[i]])
}

edges <- do.call(rbind, edge.list)

g <- graph.data.frame(edges, directed = FALSE)

h <- bipartite.projection(g, types = bipartite.mapping(g)$type)$proj2
h2 <- subgraph.edges(h, eids = E(h)[E(h)$weight > 2], delete.vertices = FALSE)

V(h2)$label.color <- "black"
V(h2)$label.cex <- 1.5
V(h2)$size <- 5
V(h2)$color <- "orange"

par(mar=c(0,0,0,0))
plot(h2)

layout.temp <- layout.circle(graph.empty(12))

g$layout <- layout.random(g)
g$layout[1:12,] <- layout.temp
g$layout <- layout.fruchterman.reingold(g, start = g$layout, niter = 5000)

V(g)$label.color <- "black"
V(g)$label.cex <- 1.5
V(g)$size <- 5
V(g)$color <- "orange"
V(g)[1:12]$color <- "red1"

par(mar=c(0,0,0,0))
plot(g)