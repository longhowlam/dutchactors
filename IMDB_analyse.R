library(readr)
library(dplyr)
library(visNetwork)
library(igraph)

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

visNetwork(nodes, edges ) %>%  
  visIgraphLayout(layout = "layout_with_graphopt")  %>% 
  visEdges(smooth = FALSE) %>% 
  visPhysics(
    solver = "forceAtlas2Based", 
    forceAtlas2Based = list(gravitationalConstant = -500)
  ) %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T)) %>% 
  visLegend()

visNetwork(nodes, edges ) %>% 
  visPhysics(
    solver = "forceAtlas2Based", 
    forceAtlas2Based = list(gravitationalConstant = -500)
  ) %>% 
  visEdges(smooth = FALSE)

####### centrality ############################################################

actors.GRP = graph_from_data_frame(edges)
plot(actors.GRP)
######## community detection #################################################


ceb <- cluster_edge_betweenness(actors.GRP) 
dendPlot(ceb, mode="hclust")
plot(
  ceb,
  actors.GRP,
  vertex.label.cex=0.1, vertex.size=1, edge.arrow.size=.01 ,
  edge.width	=0.1, edge.arrow.width	 =0.1,
  layout = layout_with_graphopt) 
