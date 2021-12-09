# input manipulation
# find ([0-9])
# replace ,\1

# part 1 ------------------------------------------------------------------
library(dplyr)

df <- read.csv('day_9_input.csv', header = FALSE)
df[,1] <- NULL

low.vector <- vector()
low.position <- vector()

for (i in 1:nrow(df)){
  for (j in 1:ncol(df)){
    lowest <- 0
    if (i > 1){
      if (df[i,j] < df[i-1,j]) lowest <- lowest + 1
    } else lowest <- lowest + 1
    if (j > 1){
      if (df[i,j] < df[i,j-1]) lowest <- lowest + 1
    } else lowest <- lowest + 1
    if (i < 100){
      if (df[i,j] < df[i+1,j]) lowest <- lowest + 1
    } else lowest <- lowest + 1
    if (j < 100){
      if (df[i,j] < df[i,j+1]) lowest <- lowest + 1
    } else lowest <- lowest + 1
    if (lowest == 4) {
      low.vector <- c(low.vector, df[i,j] + 1)
      low.position <- c(low.position, list(c(i,j)))
    }
  }
}

sum(low.vector)
# 478


# part 2 ------------------------------------------------------------------
area.vector <- vector()

check.adjacent <-function(i,j,field.vector,check.vector){
  if (i > 1){
    if (df[i,j] < df[i-1,j] & df[i-1,j]!=9) field.vector <- c(field.vector, list(c(i-1,j)))
  }
  if (j > 1){
    if (df[i,j] < df[i,j-1] & df[i,j-1]!=9) field.vector <- c(field.vector, list(c(i,j-1)))
  }
  if (i < 100){
    if (df[i,j] < df[i+1,j] & df[i+1,j]!=9) field.vector <- c(field.vector, list(c(i+1,j)))
  }
  if (j < 100){
    if (df[i,j] < df[i,j+1] & df[i,j+1]!=9) field.vector <- c(field.vector, list(c(i,j+1)))
  }
  check.vector <- c(check.vector, list(c(i,j)))
  if (sum(!field.vector %in% check.vector) > 0){
    i <- field.vector[!field.vector %in% check.vector][1][[1]][1]
    j <- field.vector[!field.vector %in% check.vector][1][[1]][2]
    return(check.adjacent(i,j,field.vector,check.vector))
  }
  return(field.vector)
}

for (l in 1:length(low.position)){
  i <- low.position[[l]][1]
  j <- low.position[[l]][2]
  field.vector <- vector()
  check.vector <- vector()
  field.vector <- c(field.vector, list(c(i,j)))
  area.vector[l] <- length(unique(check.adjacent(i,j,field.vector,check.vector)))
}

prod(area.vector[order(area.vector)][(length(area.vector)-2):(length(area.vector))])
# 1327014

