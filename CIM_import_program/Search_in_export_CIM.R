rm(list=ls())

library(ggraph)
library(igraph)
library(edgebundleR)
library(tidyverse)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(networkD3)

library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag)  
# devtools::install_github("mattflor/chorddiag")



Dane <- fread("D:/Programming/Python_Micro_Codes/CIM_import_program/export-cim.txt", sep = '\t', header = FALSE, blank.lines.skip = TRUE, fill = TRUE)
# Dane2 <- fread("D:/Programming/Python_Micro_Codes/CIM_import_program/MIKRONIKA_23A10h_Moddest.xml", header = FALSE, blank.lines.skip = TRUE, fill = TRUE)

### Podsumowanie ilosci znakow w kazdej kolumnie - w celach testowych
# kappa <- sapply(Dane,nchar)
# summary(kappa)
# znaki <- Dane[which(kappa[,3]<3),3]

# nazwy <- data.frame(Dane[1:36,])
# # d1 <- data.frame(Dane[which(Dane$V1==nazwy[1,1])])
# 
# for(i in c(1:length(nazwy[,1]))) {
#   gdzie <- which(Dane$V1==nazwy[i,1])
#   assign((paste(nazwy[i,1])), data.frame(Dane[gdzie[2:length(gdzie)],]))
#   assign((paste(nazwy[i,1])), setNames(get((paste(nazwy[i,1]))), nazwy[i,]))
# }
# 
# # for(i in c(1:length(nazwy[,1]))) {
# #   for (x in paste(nazwy[i,1])) {
# #     # print(get(x)[1:5,2])
# #     assign(c("Find_",x), )
# #     
# #   }
# #   
# # }
# 
# Asocjacje <- fread("D:/Programming/Python_Micro_Codes/CIM_import_program/asocjacje_kr.txt", sep = ',', header = TRUE, blank.lines.skip = TRUE, fill = TRUE)
# 
# 
# # create a data frame giving the hierarchical structure of your individuals
# d1 <- data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
# d2 <- data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
# edges <- rbind(d1, d2)
# 
# # create a dataframe with connection between leaves (individuals)
# all_leaves <- paste("subgroup", seq(1,100), sep="_")
# connect <- rbind( 
#   data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), 
#   data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), 
#   data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), 
#   data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
# connect$value <- runif(nrow(connect))
# 
# # create a vertices data.frame. One line per object of our hierarchy
# vertices  <-  data.frame(
#   name = unique(c(as.character(edges$from), as.character(edges$to))) , 
#   value = runif(111)
# ) 
# # Let's add a column with the group of each name. It will be useful later to color points
# vertices$group  <-  edges$from[ match( vertices$name, edges$to ) ]
# 
# #Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
# #calculate the ANGLE of the labels
# vertices$id <- NA
# myleaves <- which(is.na( match(vertices$name, edges$from) ))
# nleaves <- length(myleaves)
# vertices$id[ myleaves ] <- seq(1:nleaves)
# vertices$angle <- 90 - 360 * vertices$id / nleaves
# 
# # calculate the alignment of labels: right or left
# # If I am on the left part of the plot, my labels have currently an angle < -90
# vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)
# 
# # flip angle BY to make them readable
# vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
# 
# # Create a graph object
# mygraph <- igraph::graph_from_data_frame( edges, vertices=vertices )
# 
# # The connection object must refer to the ids of the leaves:
# from  <-  match( connect$from, vertices$name)
# to  <-  match( connect$to, vertices$name)
# 
# 
# # Basic usual argument
# ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
#   geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
#   geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.5, colour="skyblue", width=1) +
#   geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust), size=1.5, alpha=1) +
#   theme_void() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(c(0,0,0,0),"cm"),
#   ) +
#   expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))
# 
# ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
#   geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, width=0.9, aes(colour=..index..)) +
#   scale_edge_colour_distiller(palette = "RdPu") +
#   
#   geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2, alpha=1) +
#   
#   geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
#   scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
#   scale_size_continuous( range = c(0.1,10) ) +
#   
#   theme_void() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(c(0,0,0,0),"cm"),
#   ) +
#   expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
# 
# 
# 
# 
# 
# 
# 
# ### --- Moja wersja --- ### ------------------------------------------------------------------------------------------------
# 
# ### create a data frame giving the hierarchical structure of your individuals
# # d1 <- data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
# # d2 <- data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
# # edges <- rbind(d1, d2)
# edges <- NULL
# liscie <- NULL
# origin <- NULL
# parents <- NULL
# 
# for (x in c(1:length(nazwy[,1]))) {
#   for (y in c(2:length(nazwy[1,]))) {
#     # print(c(x,y))
#     if(grepl("^$", nazwy[x,y]) == FALSE){
#       # liscie <- rbind(liscie, nazwy[x,y])
#       liscie <- c(liscie, nazwy[x,y])
#       origin <- c(origin, nazwy[x,1])
#     }
#   }
# }
# 
# for (x in c(1:length(nazwy[,1]))) {
#    parents <- rbind(parents, c("origin", nazwy[x,1]))
# }
# 
# edges <- rbind(parents, cbind(origin, liscie))
# 
# 
# 
# ### create a dataframe with connection between leaves (individuals)
# # all_leaves <- paste("subgroup", seq(1,100), sep="_")
# all_leaves <- liscie
# # connect <- rbind(
# #   data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)),
# #   data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)),
# #   data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)),
# #   data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) )
# # connect$value <- runif(nrow(connect))
# connect <- Asocjacje[,3:5]
# 
# ### create a vertices data.frame. One line per object of our hierarchy
# vertices  <-  data.frame(
#   name = unique(c(as.character(edges[,1]), as.character(edges[,2]))) , 
#   value = 1
# ) 
# 
# ### Let's add a column with the group of each name. It will be useful later to color points
# vertices$group  <-  edges[match(vertices$name, edges[,2]),1]
# 
# ### Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
# ### calculate the ANGLE of the labels
# vertices$id <- NA
# myleaves <- which(is.na( match(vertices$name, edges[,1]) ))
# nleaves <- length(myleaves)
# vertices$id[ myleaves ] <- seq(1:nleaves)
# vertices$angle <- 90 - 360 * vertices$id / nleaves
# 
# # calculate the alignment of labels: right or left
# # If I am on the left part of the plot, my labels have currently an angle < -90
# vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)
# 
# # flip angle BY to make them readable
# vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)
# 
# # Create a graph object
# mygraph <- igraph::graph_from_data_frame( edges, vertices=vertices )
# 
# # The connection object must refer to the ids of the leaves:
# from  <-  match( connect$sk³adnik_z_ob1, vertices$name)
# to  <-  match( connect$sk³adnik_z_ob2, vertices$name)
# value <- match( connect$wart1, Asocjacje$wart1)
# 
# # Basic usual argument
# ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
#   geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
#   geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.5, colour="skyblue", width=1) +
#   geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust), size=1.5, alpha=1) +
#   theme_void() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(c(0,0,0,0),"cm"),
#   ) +
#   expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2))
# 
# ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
#   geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, width=0.9, aes(colour=..index..)) +
#   scale_edge_colour_distiller(palette = "RdPu") +
#   geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2, alpha=1) +
#   geom_node_point(aes(filter = leaf, x = x*1.2, y=y*1.2, colour=group, size=value, alpha=0.2)) +
#   scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
#   scale_size_continuous( range = c(0.1,5) ) +
#   
#   theme_void() +
#   theme(
#     legend.position="none",
#     plot.margin=unit(c(0,0,0,0),"cm"),
#   ) +
#   expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
# 
# 
# 
# ####### World Map
# 
# # Load dataset from github
# data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
# 
# # short names
# colnames(data) <- c("Africa", "East Asia", "Europe", "Latin Ame.",   "North Ame.",   "Oceania", "South Asia", "South East Asia", "Soviet Union", "West.Asia")
# rownames(data) <- colnames(data)
# 
# # I need a long format
# data_long <- data %>%
#   rownames_to_column %>%
#   gather(key = 'key', value = 'value', -rowname)
# 
# # parameters
# circos.clear()
# circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
# par(mar = rep(0, 4))
# 
# # color palette
# mycolor <- viridis(10, alpha = 1, begin = 0, end = 1, option = "D")
# mycolor <- mycolor[sample(1:10)]
# 
# # Base plot
# chordDiagram(
#   x = data_long, 
#   grid.col = mycolor,
#   transparency = 0.25,
#   directional = 1,
#   direction.type = c("arrows", "diffHeight"), 
#   diffHeight  = -0.04,
#   annotationTrack = "grid", 
#   annotationTrackHeight = c(0.05, 0.1),
#   link.arr.type = "big.arrow", 
#   link.sort = TRUE, 
#   link.largest.ontop = TRUE)
# 
# # Add text and axis
# circos.trackPlotRegion(
#   track.index = 1, 
#   bg.border = NA, 
#   panel.fun = function(x, y) {
#     
#     xlim = get.cell.meta.data("xlim")
#     sector.index = get.cell.meta.data("sector.index")
#     
#     # Add names to the sector. 
#     circos.text(
#       x = mean(xlim), 
#       y = 3.2, 
#       labels = sector.index, 
#       facing = "bending", 
#       cex = 0.8
#     )
#     
#     # Add graduation on axis
#     circos.axis(
#       h = "top", 
#       major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
#       minor.ticks = 1, 
#       major.tick.percentage = 0.5,
#       labels.niceFacing = FALSE)
#   }
# )
# 
# 
# ### ---------------------------------------------------------------------------------------------------------------- ###
# 
# # Load package
# # devtools::install_github("mattflor/chorddiag")
# library(chorddiag)
# library(igraph)
# library(tidygraph)
# 
# 
# 
# mig_data <- read_csv("http://download.gsb.bund.de/BIB/global_flow/Data%20on%20the%20global%20flow%20of%20people_Version%20March2014.csv")
# mig_data$region_orig <- as.factor(mig_data$region_orig)
# mig_data$country_orig <- as.factor(mig_data$country_orig)
# mig_data$region_dest <- as.factor(mig_data$region_dest)
# mig_data$country_dest <- as.factor(mig_data$country_dest)
# 
# mig_data<-mig_data %>% select(country_orig, country_dest, region_orig, region_dest,countryflow_2005 )
# 
# mig_data_filter<-mig_data %>% filter(countryflow_2005>=100000)
# mig_data_filter<-as.matrix(as_adjacency_matrix(as_tbl_graph(mig_data_filter),attr = "countryflow_2005"))
# 
# chord<-chorddiag(data = mig_data_filter,
#                  groupnamePadding = 30,
#                  groupPadding = 3,
#                  groupColors = c("#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"),
#                  groupnameFontsize = 13 ,
#                  showTicks = FALSE,
#                  margin=150,
#                  tooltipGroupConnector = "    &#x25B6;    ",
#                  chordedgeColor = "#B3B6B7"
# )
# chord
# 
# 
# Asocjacje <- fread("D:/Programming/Python_Micro_Codes/CIM_import_program/asocjacje_kr.txt", sep = ',', header = TRUE, blank.lines.skip = TRUE, fill = TRUE)
# Asocjacje$obiekt1 <- as.factor(Asocjacje$obiekt1)
# Asocjacje$obiekt1 <- as.factor(Asocjacje$obiekt1)
# Asocjacje$sk³adnik_z_ob1 <- as.factor(Asocjacje$sk³adnik_z_ob1)
# Asocjacje$sk³adnik_z_ob2 <- as.factor(Asocjacje$sk³adnik_z_ob2)
# Asocjacje$wart1 <- (Asocjacje$wart1)
# 
# 
# Asoc <- data.frame(Asocjacje %>% select(sk³adnik_z_ob1, sk³adnik_z_ob2, obiekt1, obiekt2))
#   
# kappa_wart1 <- match(Asocjacje$wart1, Asocjacje$wart1)
# # kappa_wart1 <- as.factor(Asocjacje$wart1)
# Asoc <- data.frame(Asoc, kappa_wart1)
# 
# mig_data_filtr_Aso <- as.matrix(as_adjacency_matrix(as_tbl_graph(Asoc),attr = "kappa_wart1"))
# 
# chord<-chorddiag(data = mig_data_filtr_Aso,
#                  groupnamePadding = 30,
#                  groupPadding = 3,
#                  groupnameFontsize = 13 ,
#                  showTicks = T, showGroupnames = T,
#                  margin=150,
# )
# chord
# 
# 
# 


# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/chord_interactive.html"))


### -------------------------- Network -------------------------------------------------------------------------------- ###
# 
# # Libraries
# library(igraph)
# library(networkD3)
# 
# # create a dataset:
# data <- tibble(
#   from=c("A", "A", "B", "D", "C", "D", "E", "B", "C", "D", "K", "A", "M"),
#   to=c("B", "E", "F", "A", "C", "A", "B", "Z", "A", "C", "A", "B", "K")
# )
# 
# # Plot
# p <- simpleNetwork(Asocjacje, height="100px", width="100px",        
#                    Source = 3,                 # column number of source
#                    Target = 4,                 # column number of target
#                    linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
#                    charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
#                    fontSize = 14,               # size of the node names
#                    fontFamily = "serif",       # font og node names
#                    linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
#                    nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
#                    opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
#                    zoom = T                    # Can you zoom on the figure?
# )
# 
# p
# 
# URL <- paste0(
#   "https://cdn.rawgit.com/christophergandrud/networkD3/",
#   "master/JSONdata/energy.json")
# Energy <- jsonlite::fromJSON(URL)
# 
# sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
#               Target = "target", Value = "value", NodeID = "name",
#               units = "TWh", fontSize = 12, nodeWidth = 30)
# 
# sankeyNetwork(Links = FROMO, Nodes = NODO, Source = "from",
#               Target = "to", Value = "value", NodeID = "liscie",
#               units = "", fontSize = 12, nodeWidth = 30)
# 
# 
# 
# # save the widget
# # library(htmlwidgets)
# # saveWidget(p, file=paste0( getwd(), "/HtmlWidget/networkInteractive2.html"))
# 
# 
# from  <-  match( connect$sk³adnik_z_ob1, edges[37:length(liscie),2]) - 1
# to  <-  match( connect$sk³adnik_z_ob2, edges[37:length(liscie),2]) - 1
# value <- Asocjacje$wart1
# # value <- match( connect$wart1, Asocjacje$wart1)
# 
# FROMO <- data.frame(from, to, value)
# NODO <- data.frame(edges[37:length(liscie),])
# 
# # Plot
# 
# forceNetwork(Links = FROMO, Nodes = NODO,
#              Source = "from", Target = "to",
#              Value = "value", NodeID = "liscie", Group = "origin",
#              opacity = 0.8, zoom = TRUE, clickAction = TRUE, arrows = TRUE)
# 
# 
# 
# 
# 
# ### --- NETWORK --- ###
# 
# wszystkie_nazwy <- edges[,1]
# wszystkie_nazwy <- rbind(edges[,2])
# wszystkie_nazwy <- unique(as.vector(wszystkie_nazwy))
# 
# fromvis <- match(Asocjacje$sk³adnik_z_ob1, wszystkie_nazwy)
# tovis <- match(Asocjacje$sk³adnik_z_ob2, wszystkie_nazwy)
# 
# 
# library(visNetwork)
# 
# 
# nodes <- data.frame(id = fromvis, 
#                     label = paste("Node", 1:length(fromvis)),                                 # add labels on nodes
#                     group = ,                                     # add groups on nodes 
#                     value = 1:10)                                                # size adding value
# 
# 
# edges <- data.frame(from = fromvis, to = tovis,
#                     label = Asocjacje$wart1,                                 # add labels on edges
#                     length = c(100,500),                                        # length
#                     arrows = c("to", "from"))                                   # arrows
# 
# 
# visNetwork(nodes, edges, width = "100%")



### -------------------------------------------------------------------------------------------------------------------- ###

rm(list=ls())

library(ggraph)
library(igraph)
library(edgebundleR)
library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(networkD3)
library(data.table)
library(gtools)


library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag) 
library(visNetwork)

Dane <- fread("D:/Programming/Python_Micro_Codes/CIM_import_program/export-cim.txt", sep = '\t', header = FALSE, blank.lines.skip = TRUE, fill = TRUE)
# Asocjacje <- fread("D:/Programming/Python_Micro_Codes/CIM_import_program/asocjacje_kr.txt", sep = ',', header = TRUE, blank.lines.skip = TRUE, fill = TRUE)
# 
# kappa <- (paste(Asocjacje$obiekt1, "=", Asocjacje$sk³adnik_z_ob1))
# kappa <- cbind(kappa, paste(Asocjacje$obiekt2, "=", Asocjacje$sk³adnik_z_ob2))
# wszystkie <- unique(as.vector(kappa))
# kappa <- data.frame(kappa)
# 
# ### Simple network
# simpleNetwork(kappa, height="100px", width="100px",        
#                    Source = 1,                 # column number of source
#                    Target = 2,                 # column number of target
#                    linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
#                    charge = -5000,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
#                    fontSize = 14,               # size of the node names
#                    fontFamily = "serif",       # font og node names
#                    linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
#                    nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
#                    opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
#                    zoom = T                    # Can you zoom on the figure?
# )
# 
# 
# unikalne <- unique(Asocjacje$wart1)
# paths <- data.frame(stringsAsFactors = FALSE)
# ### Paths
# for (x in c(1:length(unikalne))) {
#   print(unikalne[x])
#   wystapienia <- which(Asocjacje$wart1 == unikalne[x])
#   print(wystapienia)
#   
#   paths <- rbind(paths, c(unikalne[x], paste(wystapienia, collapse = ",")),stringsAsFactors = FALSE)
#   
#   for (y in c(1:length(wystapienia))) {
#     # print(c(wystapienia[y], Asocjacje$sk³adnik_z_ob1[wystapienia[y]], "<->", Asocjacje$sk³adnik_z_ob2[wystapienia[y]]))
#     print(paste(as.character(Asocjacje[wystapienia[y],1:2]), "=", as.character(Asocjacje[wystapienia[y],3:4])))
#     kelo <- (paste(as.character(Asocjacje[wystapienia[y],1:2]), "=", as.character(Asocjacje[wystapienia[y],3:4])))
#     paths <- rbind(paths, kelo, stringsAsFactors = FALSE)
#   }
#   
#   cat('\n')
#   # paths <- rbind(paths,which(Asocjacje$wart1 == Asocjacje$wart1[x]))
# }
# 




### --- Drzewko - sciezka - aby sprawdzic gdzie co idzie --- ###

Dane <- fread("D:/Programming/Python_Micro_Codes/CIM_import_program/export-cim.txt", sep = '\t', header = FALSE, blank.lines.skip = TRUE, fill = TRUE)

nazwy <- data.frame(Dane[1:36,])
# d1 <- data.frame(Dane[which(Dane$V1==nazwy[1,1])])

for(i in c(1:length(nazwy[,1]))) {
  gdzie <- which(Dane$V1==nazwy[i,1])
  assign((paste(nazwy[i,1])), data.frame(Dane[gdzie[2:length(gdzie)],]))
  assign((paste(nazwy[i,1])), setNames(get((paste(nazwy[i,1]))), nazwy[i,]))
}

Stacje <- `cim:Substation`


# ID Stacji = BDJ21989
BDJ21989_ID <- Stacje[which(Stacje[,3] == "BDJ21989"),2]
Stacja_Start <- Stacje[which(Stacje[,3] == "BDJ21989"),]

# GPZ_Bedzin_ID <- Stacje[which(Stacje[,3] == ""),2]


# Znalezienie wszystkich przypadkow dla nazwy = BDJ21989
for(i in nazwy[,1]) {
  # print(i)
  print(i)
  print(which(array(grepl('BDJ21989|BDJ21989.|.BDJ21989.',as.matrix(get(i))),dim(get(i))),T))
  
  # for (x in get(i)[,1:length(get(i))]) {
  #   # print(x)
  #   # print(c(i, which(x==BDJ21989_ID)))
  #   # print(c(i, which(array(grepl('BDJ21989',as.matrix(i)),dim(get(i))),T)))
  # }
  # print(which(i == ))
  # assign((paste(nazwy[i,1])), data.frame(Dane[gdzie[2:length(gdzie)],]))
  # assign((paste(nazwy[i,1])), setNames(get((paste(nazwy[i,1]))), nazwy[i,]))
}

# print(which(array(grepl('BDJ21989|BDJ21989.|.BDJ21989.' ,as.matrix(Stacje)),dim(Stacje)),T))
# print(which(array(grepl('BDJ21989|BDJ21989.|.BDJ21989.',as.matrix(`cim:PowerTransformer`)),dim(`cim:PowerTransformer`)),T))

obiekty <- nazwy[,1]
# obiekty <- obiekty[-str_detect(obiekty, i)]
exclude <- c("cim:PositionPoint", "cim:Location", "cim:VoltageLevel", "cim:Discrete", "cim:DiscreteValue", "cim:Bay", "cim:SubGeographicalRegion")
obiekty_zred <- obiekty[! obiekty %in% exclude]
obiekty_zred_Sub <- obiekty[! obiekty %in% c(exclude, "cim:Substation")]

### Dzialajaca petla szukajaca - szuka na poziomie lvl-2 ID - czyli znalezionych na lvl-1 "cim:IdentifiedObject.name"

# for(i in obiekty_zred) {
#   # print(i)
#   gdzie <- which(array(grepl(BDJ21989_ID,as.matrix(get(i))),dim(get(i))),T)
# 
#   if(length(gdzie)>0){
#     print(i)
#     print(gdzie)
#     unikat <- unique(gdzie[,1])
#     print(unikat)
#     print(get(i)[unikat,])
#     kolumienka <- which(colnames(get(i)) == "cim:IdentifiedObject.name")
#     ID <- (get(i)[unikat,kolumienka])
# 
#     for(li in obiekty_zred[-str_detect(obiekty, i)]) {
#       # str_detect(colnames(get(li)), "cim:IdentifiedObject.name")
#       # str_detect(colnames(get(li)), "cim:IdentifiedObject.name")
#       print(li)
#       # gdzie2 <- which(array(grepl(paste(paste0(".",ID,"."),paste0(".",ID),paste0(ID,"."), sep = "|"), as.matrix(get(li)), fixed = F),dim(get(li))),T)
#       gdzie2 <- which(array(grepl(ID, as.matrix(get(li)), fixed = F),dim(get(li))),T)
#       unikat2 <- unique(gdzie2[,1])
#       print(gdzie2)
#     }
#   }
#   # print(which(array(grepl(BDJ21989_ID,as.matrix(get(i))),dim(get(i))),T))
# }



### Zrobiæ test na szukiwanie = get(i)[unikat,] = kazdego elementu wszedzie w FOR level_2

# for(i in obiekty_zred) {
#   # print(i)
#   gdzie <- which(array(grepl(BDJ21989_ID,as.matrix(get(i))),dim(get(i))),T)
#   
#   if(length(gdzie)>0){
#     print("")
#     print(i)
#     print(gdzie) 
#     unikat <- unique(gdzie[,1])
#     print(unikat)
#     print(get(i)[unikat,])
#     # kolumienka <- which(colnames(get(i)) == "cim:IdentifiedObject.name")
#     IDs <- (get(i)[unikat,])
#     
#     for (x in IDs[,-1]) {
#       obiekt_teraz <- as.character(IDs[1])
#       if(x != ""){
#         print("")
#         print(c("LEVEL=2: ", x))
#         
#         for (k in obiekty[! obiekty %in% obiekt_teraz]) {
#           gdzie2 <- which(array(grepl(paste( "#" , x ,sep = ""), as.matrix(get(k)), fixed = TRUE),dim(get(k))),T)
#           if(length(gdzie2) > 0){
#             print(k)
#             print(gdzie2)
#             unikat2 <- unique(gdzie2[,1])
#             IDs <- get(k)[unikat2,]
#             print(IDs)
#           }
#         }
#       }
#     }
# 
#     
#     # for(li in obiekty_zred[-str_detect(obiekty, i)]) {
#     #   # str_detect(colnames(get(li)), "cim:IdentifiedObject.name")
#     #   # str_detect(colnames(get(li)), "cim:IdentifiedObject.name")
#     #   print(li)
#     #   # gdzie2 <- which(array(grepl(paste(paste0(".",ID,"."),paste0(".",ID),paste0(ID,"."), sep = "|"), as.matrix(get(li)), fixed = F),dim(get(li))),T)
#     #   unikat2 <- unique(gdzie2[,1])
#     #   print(gdzie2)
#     # }
#   }
#   # print(which(array(grepl(BDJ21989_ID,as.matrix(get(i))),dim(get(i))),T))
# }

# cloneEnv <- function(envir, deep = T) {
#   if(deep) {
#     clone <- list2env(rapply(as.list(envir, all.names = TRUE), cloneEnv, classes = "environment", how = "replace"), parent = parent.env(envir))
#   } else {
#     clone <- list2env(as.list(envir, all.names = TRUE), parent = parent.env(envir))
#   }
#   attributes(clone) <- attributes(envir)
#   return(clone)
# }
# 
# 
# rekurenkcja <- function(IDs, obiekty_zred, lvl){
#   
#   # print(IDs[2] %in% widzialem[,2])
#   assign(paste0("e",lvl), cloneEnv(e, deep = T), envir = .GlobalEnv)
#   
#   
#   # if(length(which(IDs[2] %in% widzialem[,2])) > 0){
#   if(IDs[2] %in% widzialem[,2]){
#     print("--- KURWA WIDZIALEM ---")
#     print(IDs[2])
#     # print(widzialem)
#     # return(1)
#     
#   }else{
#     
#     
#   obiekt_teraz <- as.character(IDs[1])
#   
#   if(obiekt_teraz == "cim:Substation"){
#       print("---------- UPS -------------")
#       print(obiekt_teraz)
#       print(IDs)
#       return(1)
#       break
#   }else{
#     
#     
#     colnames(IDs) <- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
#     assign(e$widzialem, rbind.fill(widzialem, IDs), envir = e)
#     
#     print(c("Wrzucone IDs do widzianych:"))
#     print(IDs)
#     print(c("Wypisanie widzianych:", length(widzialem[,2])))
#   
#   for (x in IDs[,2]) {
#   # for (x in IDs[,-1]) {
#    
#     if(x != ""){
#       cat("")
#       print(c("LEVEL=", lvl,":", x))
#       print(c("Szukamy:",paste( "#" , x ,sep = "")))
#       print(c("Szukamy:",paste(str_remove(x,"#"))))
#       
#       for (k in obiekty_zred[! obiekty_zred %in% obiekt_teraz]) {
#         # print(c("Hello: ", k))
#         # print(c("Szukamy:",paste( "#" , x ,sep = ""), "w ", k))
#         gdzie2 <- which(array(grepl( paste0( "#" , x), as.matrix(get(k)), fixed = TRUE),dim(get(k))),T)
#         
#         # Trzeba omijac te rzeczy ktore juz byly
#         
#         if(length(gdzie2) > 0){
#           print(c("Szukamy:",paste( "#" , x ,sep = "")))
#           print(k)
#           # print(gdzie2)
#           print(c("Ile znaleziono obiektow:" ,length(gdzie2[,1])))
#           unikat2 <- unique(gdzie2[,1])
#           IDs <- get(k)[unikat2,]
#           assign(paste0("IDs_",lvl), get(k)[unikat2,])
#           # print(IDs)
#           for (q in c(1:length(get(paste0("IDs_",lvl))[,1]))) {
#             print("kappa")
#             # widzialem <- rbind.fill(IDs)
#             return(rekurenkcja(get(paste0("IDs_",lvl))[q,], obiekty_zred, lvl+1))
#           }
#           # return(rekurenkcja(IDs, obiekty_zred, lvl+1))
#         }
#         
#         # print(c("Szukamy:",paste(str_remove(x,"#"))))
#         gdzie3 <- which(array(grepl( paste0( str_remove(x,"#")), as.matrix(get(k)), fixed = TRUE),dim(get(k))),T)
#         
#         if(length(gdzie3) > 0){
#           print(c("Szukamy:",paste(str_remove(x,"#"))))
#           print(k)
#           # print(gdzie3)
#           print(c("Ile znaleziono obiektow:" , length(gdzie3[,1])))
#           unikat3 <- unique(gdzie3[,1])
#           IDs <- get(k)[unikat3,]
#           assign(paste0("IDs_",lvl), get(k)[unikat3,])
#           # print(IDs)
#           for (t in c(1:length(get(paste0("IDs_",lvl))[,1]))) {
#             print("kappa")
#             # widzialem <- rbind.fill(IDs)
#             return(rekurenkcja(get(paste0("IDs_",lvl))[t,], obiekty_zred, lvl+1))
#           }
#           # return(rekurenkcja(IDs, obiekty_zred, lvl+1))
#         }
#       }
#     }
#   }
#   }
#   }
#   
# }
# 



### --- Test z REKURENCJA --- ###
Stacja_Start <- Stacje[which(Stacje[,3] == "BDJ21989"),]
colnames(Stacja_Start) <- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")

widzialem <- data.frame(Stacja_Start, fix.empty.names = F)
colnames(widzialem) <- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
widzialem <- widzialem[-1,]
# widzialem <- rbind.fill(widzialem, (Stacja_Start))
# 
# Test <- `cim:Terminal`[32,]
# colnames(Test) <- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
# widzialem <- rbind.fill(widzialem, Test)


do_przejrzenia <- data.frame(Stacja_Start, fix.empty.names = F)
colnames(do_przejrzenia) <- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
# do_prze <- do_przejrzenia[-1,]
# length(do_prze[,1])
# do_przejrzenia <- do_przejrzenia[-1,]
print(do_przejrzenia)
IDs <<- do_przejrzenia
# do_przejrzenia <- do_przejrzenia[-1,]
# elko <- 1
stacja <- T

while ( length(do_przejrzenia[,1]) > 0 ) {
  
  print(c("--- WHILE ------ Dlugosc :: do_przejrzenia :: ", length(do_przejrzenia[,1])))
  
  print(do_przejrzenia)
  IDs <<- do_przejrzenia[1,]
  do_przejrzenia <<- do_przejrzenia[-1,]
  
  # if(length(which(IDs[2] %in% widzialem[,2])) > 0){
  if(IDs[2] %in% widzialem[,2]){
    print("--- KURWA WIDZIALEM ---")
    print(IDs[2])
    # print(widzialem)
    # return(1)
    next
    
  }else{
    
    
    obiekt_teraz <<- as.character(IDs[1])
    print(obiekt_teraz)
    if(obiekt_teraz == "cim:Substation"){
      stacja <- !stacja
    }
    
    
    if(stacja==T && obiekt_teraz == "cim:Substation"){
      print("---------- UPS -------------")
      print(obiekt_teraz)
      print(IDs)

      # break
    }else{
    
    
    colnames(IDs) <- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
    widzialem <<- rbind.fill(widzialem, IDs)
    
    print(c("Wrzucone IDs do widzianych:"))
    print(IDs)
    print(c("Wypisanie widzianych:", length(widzialem[,2])))
    
    # for (x in IDs) {
    for (x in IDs[,-1]) {
      
      print(x)
      
      if(x != "" && str_length(x)>7){
        cat("")
        print(c("LEVEL=",":", x))
        print(c("Szukamy:",paste( "#" , x ,sep = "")))
        print(c("Szukamy:",paste(str_remove(x,"#"))))
        
        for (k in obiekty_zred[! obiekty_zred %in% obiekt_teraz]) {
          # print(c("Hello: ", k))
          # print(c("Szukamy:",paste( "#" , x ,sep = ""), "w ", k))
          if(str_detect(x, "#")){
            gdzie2 <<- which(array(grepl( paste0("\\<", x, "\\>"), as.matrix(get(k)), fixed = F),dim(get(k))),T)
            teraz <- paste0("\\<", x, "\\>")
          }else{
            gdzie2 <<- which(array(grepl( paste0("\\<", "#" , x, "\\>"), as.matrix(get(k)), fixed = F),dim(get(k))),T)
            teraz <- paste0("\\<", "#" , x, "\\>")
          }
          
          
          # Trzeba omijac te rzeczy ktore juz byly
          
          if(length(gdzie2) > 0){
            print(c("Szukamy:",teraz))
            print(k)
            # print(gdzie2)
            print(c("Ile znaleziono obiektow:" ,length(gdzie2[,1])))
            unikat2 <<- unique(gdzie2[,1])
            IDs <<- get(k)[unikat2,]
            colnames(IDs) <- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
            # assign(paste0("IDs_",lvl), get(k)[unikat2,])
            # print(IDs)
            for (q in c(1:length(IDs[,1]))) {
              print(teraz)
              # widzialem <- rbind.fill(IDs)
              # return(rekurenkcja(get(paste0("IDs_",lvl))[q,], obiekty_zred, lvl+1))
              if(IDs[q,2] %in% do_przejrzenia[,2])
              { 
                print("Bylo juz")
              }else{
                do_przejrzenia <<- rbind.fill(do_przejrzenia, IDs[q,])
              }
              
            }
            # return(rekurenkcja(IDs, obiekty_zred, lvl+1))
          }
          
          # print(c("Szukamy:",paste(str_remove(x,"#"))))
          gdzie3 <<- which(array(grepl( paste0("\\<", str_remove(x,"#"), "\\>"), as.matrix(get(k)), fixed = F),dim(get(k))),T)
          teraz <- paste0("\\<", str_remove(x,"#"), "\\>")
          
          if(length(gdzie3) > 0){
            print(c("Szukamy:", teraz))
            print(k)
            # print(gdzie3)
            print(c("Ile znaleziono obiektow:" , length(gdzie3[,1])))
            unikat3 <<- unique(gdzie3[,1])
            IDs <<- get(k)[unikat3,]
            colnames(IDs) <- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
            # assign(paste0("IDs_",lvl), get(k)[unikat3,])
            # print(IDs)
            for (t in c(1:length(IDs[,1]))) {
              print(teraz)
              # widzialem <- rbind.fill(IDs)
              # return(rekurenkcja(get(paste0("IDs_",lvl))[t,], obiekty_zred, lvl+1))
              if(IDs[t,2] %in% do_przejrzenia[,2])
              {
                print("Bylo juz")
              }else{
                do_przejrzenia <<- rbind.fill(do_przejrzenia, IDs[t,])
              }
              
            }
            # return(rekurenkcja(IDs, obiekty_zred, lvl+1))
          }
        }
      }
    }
  }
  }
  # }
}






### Petla z interakcja

# for(i in obiekty_zred_Sub) {
#   # print(i)
#   gdzie <- which(array(grepl(BDJ21989_ID,as.matrix(get(i))),dim(get(i))),T)
#   
#   if(length(gdzie)>0){
#     print("")
#     print(i)
#     print(gdzie)
#     unikat <- unique(gdzie[,1])
#     print(unikat)
#     print(get(i)[unikat,])
#     # kolumienka <- which(colnames(get(i)) == "cim:IdentifiedObject.name")
#     IDs <<- (get(i)[unikat,])
#     lvl = 2
#     # print(do_przejrzenia)
#     colnames(IDs) <- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
#     # do_przejrzenia <<- IDs
#     do_przejrzenia <<- rbind.fill(do_przejrzenia, IDs)
#     
#     print(do_przejrzenia)
#     print(c("--------- Dlugosc :: do_przejrzenia :: ", length(do_przejrzenia[,1])))
#     while ( length(do_przejrzenia[,1]) > 0 ) {
#       
#       print(c("--- WHILE ------ Dlugosc :: do_przejrzenia :: ", length(do_przejrzenia[,1])))
#     
#       print(do_przejrzenia)
#     IDs <<- do_przejrzenia[1,]
#     # do_przejrzenia <<- do_przejrzenia[-1,]
# 
#     if(length(which(IDs[2] %in% widzialem[,2])) > 0){
#       if(IDs[2] %in% widzialem[,2]){
#         print("--- KURWA WIDZIALEM ---")
#         print(IDs[2])
#         # print(widzialem)
#         # return(1)
# 
#       }else{
# 
# 
#       obiekt_teraz <<- as.character(IDs[1])
#       print(obiekt_teraz)
# 
#       if(obiekt_teraz == "cim:Substation"){
#           print("---------- UPS -------------")
#           print(obiekt_teraz)
#           print(IDs)
#           # break
#       }else{
# 
# 
#         colnames(IDs) <<- c("Object_Type","rdf_ID","V3","V4","V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18")
#         widzialem <<- rbind.fill(widzialem, IDs)
# 
#         print(c("Wrzucone IDs do widzianych:"))
#         print(IDs)
#         print(c("Wypisanie widzianych:", length(widzialem[,2])))
# 
#       # for (x in IDs) {
#       for (x in IDs[,-1]) {
#         
#         print(x)
# 
#         if(x != ""){
#           cat("")
#           print(c("LEVEL=", lvl,":", x))
#           print(c("Szukamy:",paste( "#" , x ,sep = "")))
#           print(c("Szukamy:",paste(str_remove(x,"#"))))
# 
#           for (k in obiekty_zred[! obiekty_zred %in% obiekt_teraz]) {
#             # print(c("Hello: ", k))
#             # print(c("Szukamy:",paste( "#" , x ,sep = ""), "w ", k))
#             gdzie2 <<- which(array(grepl( paste0( "#" , x), as.matrix(get(k)), fixed = TRUE),dim(get(k))),T)
# 
#             # Trzeba omijac te rzeczy ktore juz byly
# 
#             if(length(gdzie2) > 0){
#               print(c("Szukamy:",paste( "#" , x ,sep = "")))
#               print(k)
#               # print(gdzie2)
#               print(c("Ile znaleziono obiektow:" ,length(gdzie2[,1])))
#               unikat2 <<- unique(gdzie2[,1])
#               IDs <<- get(k)[unikat2,]
#               # assign(paste0("IDs_",lvl), get(k)[unikat2,])
#               # print(IDs)
#               for (q in c(1:length(IDs[,1]))) {
#                 print("kappa")
#                 # widzialem <- rbind.fill(IDs)
#                 # return(rekurenkcja(get(paste0("IDs_",lvl))[q,], obiekty_zred, lvl+1))
#                 do_przejrzenia <<- rbind.fill(do_przejrzenia, IDs[q,])
#               }
#               # return(rekurenkcja(IDs, obiekty_zred, lvl+1))
#             }
# 
#             # print(c("Szukamy:",paste(str_remove(x,"#"))))
#             gdzie3 <<- which(array(grepl( paste0( str_remove(x,"#")), as.matrix(get(k)), fixed = TRUE),dim(get(k))),T)
# 
#             if(length(gdzie3) > 0){
#               print(c("Szukamy:",paste(str_remove(x,"#"))))
#               print(k)
#               # print(gdzie3)
#               print(c("Ile znaleziono obiektow:" , length(gdzie3[,1])))
#               unikat3 <<- unique(gdzie3[,1])
#               IDs <<- get(k)[unikat3,]
#               # assign(paste0("IDs_",lvl), get(k)[unikat3,])
#               # print(IDs)
#               for (t in c(1:length(IDs[,1]))) {
#                 print("kappa")
#                 # widzialem <- rbind.fill(IDs)
#                 # return(rekurenkcja(get(paste0("IDs_",lvl))[t,], obiekty_zred, lvl+1))
#                 do_przejrzenia <<- rbind.fill(do_przejrzenia, IDs[q,])
#               }
#               # return(rekurenkcja(IDs, obiekty_zred, lvl+1))
#             }
#           }
#         }
#       }
#       }
#       }
#     }
#   }
# 
#   }
# }





# e <- cloneEnv(environment(), deep = T)

# for(i in obiekty_zred_Sub) {
#   # print(i)
#   gdzie <- which(array(grepl(BDJ21989_ID,as.matrix(get(i))),dim(get(i))),T)
#   
#   if(length(gdzie)>0){
#     print("")
#     print(i)
#     print(gdzie)
#     unikat <- unique(gdzie[,1])
#     print(unikat)
#     print(get(i)[unikat,])
#     # kolumienka <- which(colnames(get(i)) == "cim:IdentifiedObject.name")
#     IDs <- (get(i)[unikat,])
#     lvl = 2
#     
#     # for (x in IDs[,-1]) {
#     #   obiekt_teraz <- as.character(IDs[1])
#     #   if(x != ""){
#     #     print("")
#     #     print(c("LEVEL=2: ", x))
#     #     
#     #     for (k in obiekty[! obiekty %in% obiekt_teraz]) {
#     #       gdzie2 <- which(array(grepl(paste( "#" , x ,sep = ""), as.matrix(get(k)), fixed = TRUE),dim(get(k))),T)
#     #       if(length(gdzie2) > 0){
#     #         print(k)
#     #         print(gdzie2)
#     #         unikat2 <- unique(gdzie2[,1])
#     #         IDs <- get(k)[unikat2,]
#     #         print(IDs)
#     #       }
#     #     }
#     #   }
#     # }
#     
#     # widzialem <- rbind(widzialem,IDs)
#     # widzialem <- Stacja_Start
#     
#     rekurenkcja(IDs, obiekty_zred, lvl)
#     
#     
#     # for(li in obiekty_zred[-str_detect(obiekty, i)]) {
#     #   # str_detect(colnames(get(li)), "cim:IdentifiedObject.name")
#     #   # str_detect(colnames(get(li)), "cim:IdentifiedObject.name")
#     #   print(li)
#     #   # gdzie2 <- which(array(grepl(paste(paste0(".",ID,"."),paste0(".",ID),paste0(ID,"."), sep = "|"), as.matrix(get(li)), fixed = F),dim(get(li))),T)
#     #   unikat2 <- unique(gdzie2[,1])
#     #   print(gdzie2)
#     # }
#   }
#   # print(which(array(grepl(BDJ21989_ID,as.matrix(get(i))),dim(get(i))),T))
# }











