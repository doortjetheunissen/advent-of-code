library(tidyverse)
library(raster)
library(Matrix)


file_name <- 'day_15_test.csv'
file_name <- 'day_15_input.csv'


risk.mat <- read.csv(file_name, header = FALSE, colClasses = c('character'), col.names = c('risk_level')) %>%
  mutate(x = strsplit(risk_level, ''),
         risk_level = NULL) %>%
  unnest_wider(x) %>%
  mutate_all(as.numeric) %>%
  as.matrix

risk.raster <- raster(risk.mat)
risk.adj <- layer_adjacency(risk.raster)



layer_adjacency <- function (layer,
                             from=which(raster::values(!is.na(layer))),
                             to=from,
                             directions=4,
                             normalize=FALSE
) {
  ij <- raster::adjacent(layer,cells=from,target=to,directions=directions,pairs=TRUE,sorted=TRUE)
  # stopifnot( all(ij[,1] != ij[,2]) ) ## NO DIAGONAL
  # note columns are (to,from)
  adj <- Matrix::sparseMatrix( i=match(ij[,2],from), j=match(ij[,1],to), x=1.0 )
  if (normalize) {
    adj <- sweep(adj,1,Matrix::rowSums(adj),"/")
  }
  return(adj)
}
