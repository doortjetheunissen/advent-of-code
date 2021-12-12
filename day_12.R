# part 1 ------------------------------------------------------------------
library(igraph)
library(tidyverse)

df <- read.csv('day_12_input.csv', col.names = c('edge'), header = FALSE) %>%
  mutate(edge.split = strsplit(edge,'-'),
         edge = NULL) %>%
  unnest_wider(edge.split) %>%
  as.matrix()
  
caves <- graph_from_edgelist(df, directed = FALSE)

# plot(caves, vertex.label.dist=2, vertex.size = 10)

path.list <- list()

add.adjacent <- function(path.vc,vertex.name){
  adjacent.vc <- names(adjacent_vertices(caves, vertex.name)[[1]])
  for (i in 1:length(adjacent.vc)){
    new.path.vc <- c(path.vc,adjacent.vc[i])
    if (!(adjacent.vc[i] %in% path.vc & adjacent.vc[i] != toupper(adjacent.vc[i])) &
        adjacent.vc[i] != 'end'){
      add.adjacent(new.path.vc,adjacent.vc[i])
    } else if (adjacent.vc[i] == 'end') {
      path.list <<- c(path.list, list(new.path.vc))
    }
  }
}

add.adjacent('start','start')

length(path.list)
# 5076


# part 2 ------------------------------------------------------------------
# uses df and caves from part 1

path.count <- 0

extended.add.adjacent <- function(path.vc,vertex.name){
  adjacent.vc <- names(adjacent_vertices(caves, vertex.name)[[1]])
  for (i in 1:length(adjacent.vc)){
    new.path.vc <- c(path.vc,adjacent.vc[i])
    if (adjacent.vc[i] == 'end'){
      path.count <<- path.count + 1
      #if (path.count %% 1000 == 0){
      #  print(path.count)
      #}
    } else if (adjacent.vc[i] == toupper(adjacent.vc[i])){
      extended.add.adjacent(new.path.vc,adjacent.vc[i])
    } else if (!((sum(path.vc[duplicated(path.vc)] != toupper(path.vc[duplicated(path.vc)])) > 0) & adjacent.vc[i] %in% path.vc) &
               adjacent.vc[i] != 'start'){
      extended.add.adjacent(new.path.vc,adjacent.vc[i])
    }
  }
}   

extended.add.adjacent('start','start')

path.count
# 145643
