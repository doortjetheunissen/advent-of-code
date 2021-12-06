# part 1 ------------------------------------------------------------------

library(dplyr)
library(magrittr)

df <- read.csv('day_5_input.csv',header = FALSE, col.names = c('x1','y1','x2','y2'))


df.horizontal <- df %>% filter(x1 == x2) %>% 
  rowwise %>% mutate(positions = list(seq(min(y1,y2), max(y1,y2), by = 1))) %>%
  mutate(xy = list(paste0(x1, ',', positions)))


df.positions <- data.frame(matrix('', ncol = 1)) %>% set_colnames(c('position'))

for (i in 1:nrow(df.horizontal)){
  df.temp <- data.frame(unlist(df.horizontal[i,'xy']), row.names = NULL) %>% set_colnames(c("position"))
  df.positions <- rbind(df.positions, df.temp)
}


df.vertical <- df %>% filter(y1 == y2) %>% 
  rowwise %>% mutate(positions = list(seq(min(x1,x2), max(x1,x2), by = 1))) %>%
  mutate(xy = list(paste0(positions, ',', y1)))


for (i in 1:nrow(df.vertical)){
  df.temp <- data.frame(unlist(df.vertical[i,'xy']), row.names = NULL) %>% set_colnames(c("position"))
  df.positions <- rbind(df.positions, df.temp)
}


df.positions[duplicated(df.positions$position),] %>% n_distinct()
#5197


# part 2 ------------------------------------------------------------------

df.diagonal <- df %>% filter(y1 != y2 & x1 != x2) %>%
  rowwise %>% mutate(x.positions = list(seq(x1,x2, by = (x1-x2)/-abs(x1-x2)))) %>%
  mutate(y.positions = list(seq(y1,y2, by = (y1-y2)/-abs(y1-y2)))) %>%
  mutate(xy = list(paste0(x.positions, ',', y.positions)))

for (i in 1:nrow(df.diagonal)){
  df.temp <- data.frame(unlist(df.diagonal[i,'xy']), row.names = NULL) %>% set_colnames(c("position"))
  df.positions <- rbind(df.positions, df.temp)
}

df.positions[duplicated(df.positions$position),] %>% n_distinct()
#18605

