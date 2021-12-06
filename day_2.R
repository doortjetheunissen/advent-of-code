# part 1 ------------------------------------------------------------------

library(stringr)

df <- read.csv('day_2_input.csv', header = FALSE, col.names = c('motion'))
df$depth <- 0
df$horizontal <- 0

# forward +H, down +D, up -D

for (i in 1:nrow(df)){
  if (str_split(df[i,"motion"], ' ')[[1]][1] == 'forward'){
    df[i,"horizontal"] <- as.numeric(str_split(df[i,"motion"], ' ')[[1]][2])
  } else if (str_split(df[i,"motion"], ' ')[[1]][1] == 'down'){
    df[i,"depth"] <- as.numeric(str_split(df[i,"motion"], ' ')[[1]][2])
  } else {
    df[i,"depth"] <- (-1) * as.numeric(str_split(df[i,"motion"], ' ')[[1]][2])
  }
}

sum(df$depth) * sum(df$horizontal)
# 1813801


# part 2 ------------------------------------------------------------------

library(dplyr)

df <- read.csv('day_2_input.csv', header = FALSE, col.names = c('motion'))

df$horizontal.diff <- 0
df$aim.diff <- 0

for (i in 1:nrow(df)){
  if (str_split(df[i,"motion"], ' ')[[1]][1] == 'forward'){
    df[i,"horizontal.diff"] <- as.numeric(str_split(df[i,"motion"], ' ')[[1]][2])
  } else if (str_split(df[i,"motion"], ' ')[[1]][1] == 'down'){
    df[i,"aim.diff"] <- as.numeric(str_split(df[i,"motion"], ' ')[[1]][2])
  } else {
    df[i,"aim.diff"] <- (-1) * as.numeric(str_split(df[i,"motion"], ' ')[[1]][2])
  }
}

df <- df %>% mutate(aim = cumsum(aim.diff), depth.diff = aim*horizontal.diff)

sum(df$depth.diff) * sum(df$horizontal.diff)
#1960569556
