rm(list=ls())

#mypath <-  "C:/Users/ss0088/Dropbox (RRI at WVU)/Shishir/ELG/SRSA/Data Analysis/"
mypath <- "C:/Users/asus/Dropbox (RRI at WVU)/Shishir/ELG/SRSA/Data Analysis/"

setwd(mypath)

source("01_SRSAfunction.R")
source("02_startup.R")
source("03_SRSAfunction.R")

#One Time running Codes
#source("https://bioconductor.org/biocLite.R")
#biocLite("BiocInstaller")
#install_github("jokergoo/ComplexHeatmap")
source("05_complexheat.R")
#source("04_CSV_Total Requirement.R")

##
grp <- function(df) {
  df1 <- tidyA(df, val = 0.001)
  grp=graph.adjacency(df1, mode="directed", weighted = TRUE)
  #plot(degree_distribution(grp, mode="in"), log="xy")
}
grp_obj <- lapply(A, grp)
G <- grp_obj$`1997`


write.csv(A$`1997`, file = "1997_A.csv")
# Graph Descriptives
#######################################
graph_descriptive <- function(df, cutval = 0.001){
  df1 <- tidyA(df, val = cutval)
  jg=graph.adjacency(df1, mode="directed", weighted = TRUE)
  out <- as.matrix(c
                   (max(degree(jg, mode="in")),
                     max(degree(jg, mode="out")),
                     max(degree(jg, mode="all")), reciprocity(jg, mode="ratio"), dyad_census(jg), transitivity(jg), 
                     transitivity(sample_gnm(vcount(jg), ecount(jg))),
                     transitivity(sample_degseq(degree(jg, mode="out"), degree(jg, mode="in"),
                                                method="simple"))))
  rownames(out) <- c("Indegree", "Outdegree", "All", "Reciprocity", "Mutual", "Asymmetric", "Null", "Transitivity", "TranA", "TranB")
  newout <- t(out)
  #o
  return(newout)
}
graph_des <- as.matrix(do.call(rbind.data.frame, lapply(A, graph_descriptive)))
graph_des
write.csv(graph_des, file="03_Graph_Discriptives.csv")
#######################################

maxd <- function(df, cutval, mode = c("in", "out", "all")){
  df1 <- tidyA(df, val = 0.001)
  grp <- graph.adjacency(df1, mode="directed", weighted = TRUE)
  res <- V(grp)$name[degree(grp, mode=mode)==max(degree(grp, mode=mode))]
  return(res)
}
myd <- as.matrix(do.call(rbind.data.frame, lapply(A_s, function(z) maxd(z, mode = "all")))); myd
write.csv(myd, file ="04_myd.csv")

assortativity_degree(G)



graph_descriptive <- function(df, cutval = 0.01){
  df1 <- tidyA(df, val = cutval)
  jg=graph.adjacency(df1, mode="directed", weighted = TRUE)
  out <- as.matrix(c
                   (max(degree(jg, mode="in")),
                     max(degree(jg, mode="out")),
                     max(degree(jg, mode="all")), reciprocity(jg, mode="ratio"),  reciprocity(sample_gnm(vcount(jg), ecount(jg)), mode="ratio"),
                     reciprocity(sample_degseq(degree(jg, mode="out"), degree(jg, mode="in"),
                                               method="simple"), mode = "ratio"),
                     dyad_census(jg), transitivity(jg), 
                     transitivity(sample_gnm(vcount(jg), ecount(jg))),
                     transitivity(sample_degseq(degree(jg, mode="out"), degree(jg, mode="in"),
                                                method="simple"))))
  #rownames(out) <- c("Indegree", "Outdegree", "All", "Reciprocity", "Mutual", "Asymmetric", "Null", "Transitivity", "TranA", "TranB")
  newout <- t(out)
  #o
  return(newout)
}
graph_des <- as.matrix(do.call(rbind.data.frame, lapply(A, graph_descriptive)))
graph_des

V(jg)$name[degree(jg)==max(degree(jg))],

V(G)$name[degree(G)==max(degree(G))]

V(jg)$name[degree(jg)==max(degree(jg))],
graph_descriptive <- function(df, cutval = 0.01){
  df1 <- tidyA(df, val = cutval)
  jg=graph.adjacency(df1, mode="directed", weighted = TRUE)
  out <- as.matrix(c
                   (max(degree(jg, mode="in")),
                     max(degree(jg, mode="out")),
                     max(degree(jg, mode="all")), reciprocity(jg, mode="ratio"), dyad_census(jg), transitivity(jg), 
                   transitivity(sample_gnm(vcount(jg), ecount(jg))),
                   transitivity(sample_degseq(degree(jg, mode="out"), degree(jg, mode="in"),
                                              method="simple"))))
  rownames(out) <- c("Indegree", "Outdegree", "All", "Reciprocity", "Mutual", "Asymmetric", "Null", "Transitivity", "TranA", "TranB")
  newout <- t(out)
#o
  return(newout)
}
graph_des <- do.call(rbind.data.frame, lapply(A, graph_descriptive))
graph_des
write.csv(graph_des, file="03_Graph_Discriptives.csv")


descriptive1 <- function(df, tidyval = 0.001){
  df1 <- tidyA(df, val = tidyval)
  require(igraph)
  jg=igraph::graph.adjacency(df1, mode="directed", weighted = TRUE)
  out <- as.matrix(c
                   (reciprocity(jg), dyad_census(jg), transitivity(jg), 
                     transitivity(sample_gnm(vcount(jg), ecount(jg))),
                     transitivity(sample_degseq(degree(jg, mode="out"), degree(jg, mode="in"),
                                                method="simple"))))
  rownames(out) <- c("Reciprocity", "Mutual", "Asymmetric", "Null", "Transitivity", "TranA", "TranB")
  newout <- t(out)
  return(newout)
}

lapply(A, descriptive1)
descriptive1(A$`1997`)



sg <- cbind(strength(g, mode="in"),strength(g, mode="out"), strength(g, mode="all"), strength(g, mode="total"))

scatter.smooth(sg[ ,1], sg[,2])
plot(sg)

strength(g, mode="in")
  
myval <-lapply(A, function(x) descriptive1(x, tidyval = 0.00001))  
output <- do.call(rbind.data.frame, myval)
output

g <- grp_obj$`1997`
lay <- layout_with_fr(g)
id <- tkplot(g, layout = lay, canvas.width=1000, canvas.height=600)
l <- tkplot.getcoords(id) # grab the coordinates from tkplot
plot(g, layout=l, vertex.size=5,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.05)


#tk_postscript(tkp$canvas, file="/tmp/output.eps")
#tkplot.close(id)

tcltk::tkpostscript(tkp$canvas, file = "output.eps")

##############
grp <- function(df) {
  df1 <- tidyA(df, val = 0.001)
  grp=graph.adjacency(df1, mode="directed", weighted = TRUE)
  #plot(degree_distribution(grp, mode="in"), log="xy")
}
grp_obj <- lapply(A_s, grp)
g <- grp_obj$`1997`


AA <- get.adjacency(grp_obj$`1997`, sparse=FALSE)
library(network)
g <- network::as.network.matrix(AA)
library(sna)

#Produce a target diagram, centering by betweenness
par(mfrow=c(2,2))
sna::gplot.target(g, degree(g), main="Degree",
                  circ.lab = FALSE, circ.col="black",
                  usearrows = FALSE, circ.lab.cex=1, jitter=FALSE, displaylabels=TRUE,
                  boxed.labels=FALSE,
                  vertex.col="red", vertex.cex=1,
                  edge.col="white", label.cex=0.6, periph.outside.offset = 0.95)

sna::gplot.target(g, closeness(g), main="Closeness",
                  circ.lab = FALSE, circ.col="black",
                  usearrows = FALSE, circ.lab.cex=1, jitter=FALSE, displaylabels=TRUE,
                  boxed.labels=FALSE,
                  vertex.col="red", vertex.cex=1,
                  edge.col="white", label.cex=0.6, periph.outside.offset = 0.95)

sna::gplot.target(g, betweenness(g), main="Betweeness",
                  circ.lab = FALSE, circ.col="black",
                  usearrows = FALSE, circ.lab.cex=1, jitter=FALSE, displaylabels=TRUE,
                  boxed.labels=FALSE,
                  vertex.col="red", vertex.cex=1,
                  edge.col="white", label.cex=0.6, periph.outside.offset = 0.95)

sna::gplot.target(g, eigen(g), main="Eigenvalue",
                  circ.lab = FALSE, circ.col="black",
                  usearrows = FALSE, circ.lab.cex=1, jitter=FALSE, displaylabels=TRUE,
                  boxed.labels=FALSE,
                  vertex.col="red", vertex.cex=1,
                  edge.col="white", label.cex=0.6, periph.outside.offset = 0.95)


#######################


lapply(grp_obj, reciprocity)
lapply(grp_obj, dyad_census) # Mutual, asymmetric, and nyll node pairs

plot(degree_distribution(grp_obj$`1997`, mode="in"), log="xy")

g <- grp_obj$`1997`
g

##

library(networkD3)
gedge <- get.edgelist(g, names=TRUE)
src <- gedge[ ,1]
target <- gedge[ ,2]
networkData <- data.frame(src, target)

# Plot
as.data.frame(get.adjacency(g))
simpleNetwork(networkData,height = 900, width= 1000,linkDistance = 100, charge = -500,
              linkColour = "#999",fontSize = 12,
              textColour = "Black", opacity = 1,
              zoom = F)

forceNetwork(networkData)
g
