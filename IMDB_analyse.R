library(readr)
library(dplyr)
library(visNetwork)
library(igraph)
library(wordcloud)

######### import data vanuit IMDB datasets ##########################################################
IMDB_basics <- read_delim("IMDB_basics.tsv", "\t", escape_double = FALSE, trim_ws = TRUE)
IMDB_akas <- read_delim("IMDB_akas.tsv", "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  group_by(titleId) %>%
  mutate(rank = min_rank(ordering)) %>%
  filter(rank ==1)

IMDB_principals <- read_delim("IMDB_principals.tsv","\t", escape_double = FALSE, trim_ws = TRUE)
IMDB_names <- read_delim("IMDB_names.tsv","\t", escape_double = FALSE, trim_ws = TRUE)


#### Bekijk alleen de films van de laatste 25 jaar ###########################
NL_Films = IMDB_akas %>% 
  filter(
    region == "NL"
  ) %>% 
  left_join(IMDB_basics, by = c("titleId"="tconst")) %>% 
  filter(
    startYear > 1992,
    titleType == "movie" | titleType == "tvMovie"
  )

jaar = NL_Films %>% group_by(startYear) %>% summarise(n=n())

#### join zodat we per film de namen hebben van de acteurs
NL_film_crew = NL_Films %>% left_join(IMDB_principals, by = c("titleId" = "tconst" ))
NL_film_crew = NL_film_crew %>%  left_join(IMDB_names, by = c("nconst"="nconst"))


#### haal per persoon zijn category uit de data ###########################

persoonCategory = NL_film_crew %>% group_by(primaryName) %>%  summarise(group = max(category))

persoonCategory %>% group_by(group) %>% summarise(n=n())

#### link spelers (acteurs) als ze in dezelfde film hebben gespeeld 
#### tel hoe vaak dat voorkomt. Dit zijn de edges

NL_film_crew_links = NL_film_crew %>%
  select(titleId, primaryName) %>%
  full_join(
    NL_film_crew %>% select(titleId, primaryName),
    by = c("titleId" = "titleId")
  ) %>% 
  filter(
    primaryName.x != primaryName.y
  ) %>% 
  group_by( primaryName.x , primaryName.y) %>% 
  summarise(
    value = n()
  ) %>% 
  rename(
    from = primaryName.x,
    to = primaryName.y
  ) %>% 
  mutate(
    label = value,
    title = value
  )

edges = NL_film_crew_links %>% filter(value > 1)
nodes = data.frame(id = unique(c(edges$from, edges$to)))
nodes = nodes %>% mutate(label = id, title = id)
nodes = nodes %>% left_join(persoonCategory, by = c("id"= "primaryName"))
nodes$value = 1

visNetwork(nodes, edges , height = "1000px", width = "1600px") %>%  
  visIgraphLayout(layout = "layout_with_graphopt")  %>% 
  visEdges(smooth = FALSE) %>% 
  visPhysics(
    solver = "forceAtlas2Based", 
    forceAtlas2Based = list(gravitationalConstant = -500)
  ) %>% 
  visOptions(
    nodesIdSelection = TRUE,
    selectedBy = "group", 
    highlightNearest = list(enabled = T, degree = 2, hover = T)
  ) %>% 
  visLegend() %>% 
  visInteraction(navigationButtons = TRUE)


####### Degree & centrality ############################################################

## maak eerst een igraph aan
actors.GRP = graph_from_data_frame(edges)
plot(
  actors.GRP,
  vertex.label.cex=0.1, vertex.size=1, edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.1,	
  layout = layout_with_graphopt
) 

deg <- degree(actors.GRP, mode="all")
deg = betweenness(actors.GRP,directed = FALSE)
hist(deg)

degreeDF = data.frame(persoon = names(deg), centrality = deg)
row.names(degreeDF) = NULL
degreeDF = degreeDF %>% arrange( desc(centrality))


plot(
  actors.GRP, 
  vertex.size = sqrt(deg),
  vertex.label.cex=0.1,  edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.1,
  layout = layout_with_graphopt
)

betw = closeness(actors.GRP)
hist(betw)
plot(
  actors.GRP, 
  vertex.size=2000000*betw,
  vertex.label.cex=0.1,  edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.1,
  layout = layout_with_graphopt
)

######## community detection #################################################


ceb <- cluster_edge_betweenness(actors.GRP) 

tmp = membership(ceb) 
NLFILM_communities =  data.frame(
  persoon = names(tmp), 
  community = as.numeric(tmp))

COMM_STATS = NLFILM_communities %>%  group_by(community) %>%  summarise(n=n())

dendPlot(ceb, mode ="hclust", use.edge.length = TRUE, cex = 0.1, horiz=TRUE)
plot(
  ceb,
  actors.GRP,
  vertex.label.cex=0.1, vertex.size=1, edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.1,
  layout = layout_with_graphopt
)


####### wordcloud degree voor een community ####################
i = 6
groep_i = NLFILM_communities %>% filter(community == i) %>% left_join(degreeDF)
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(groep6$persoon, groep6$centrality, colors=pal)

##############################################################################
#### simpel graph ###################

edg = data.frame(
  from = c(1,1,2,3,4,5,6,8,9,10,11) ,
    to = c(12,5,5,5,5,6,7,7,7,7,7)
)
graphje = graph_from_data_frame(edg)
plot(graphje)


dg <- degree(graphje, mode="all")
cls = closeness(graphje, mode="all")
betw =betweenness(graphje, directed = FALSE)
ec = eigen_centrality(graphje)


ceb2 <- cluster_edge_betweenness(graphje) 

tmp = membership(ceb2) 
tmp2 =  data.frame(
  persoon = names(tmp), 
  community = as.numeric(tmp))


dendPlot(ceb2, mode ="hclust", use.edge.length = TRUE, cex = 0.1, horiz=TRUE)
plot(
  ceb2,
  graphje,
  vertex.label.cex=0.1, vertex.size=1, edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1, edge.color	= "Black",
  edge.label.cex = 0.1,
  layout = layout_with_graphopt
)
