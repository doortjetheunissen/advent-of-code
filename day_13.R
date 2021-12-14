# part 1 ------------------------------------------------------------------
library(dplyr)
library(stringr)

# read input
input.df <- read.csv('day_13_input.csv', col.names = c('x','y'), header = FALSE)

# get folding info --> fold.df
fold.df <- input.df[is.na(input.df[,2]),] %>%
  mutate(fold.direction = ifelse(str_detect(x, 'x'),'x','y')) %>%
  rowwise() %>% mutate(fold.line = as.numeric(str_split(x, pattern = '=')[[1]][2]) + 1,
                       x = NULL, y = NULL)

# make paper with dots --> paper.df
dots.df <- input.df[!is.na(input.df[,2]),]
dots.df$x <- as.integer(dots.df$x)
max.x <- max(dots.df$x)+1
max.y <- max(dots.df$y)+1
paper.df <- data.frame(matrix(0, nrow = max.y, ncol = max.x))
for (i in 1:nrow(dots.df)){
  paper.df[dots.df[i,2]+1,dots.df[i,1]+1] <- 1
}

# horizontal folding line
fold.up <- function(paper.df,fold.line){
  top <- paper.df[1:(fold.line - 1),]
  bottom <- paper.df[(fold.line + 1):nrow(paper.df),]
  flipped.bottom <- bottom[nrow(bottom):1,]
  row.diff <- nrow(top) - nrow(flipped.bottom)
  if (row.diff > 0) {
    add.on <- data.frame(matrix(0, nrow = row.diff, ncol = ncol(top)))
    flipped.bottom <- rbind(add.on,flipped.bottom)
  }
  paper.df <- top + flipped.bottom
  return(paper.df)
}


# vertical folding line
fold.left <- function(paper.df,fold.line){
  left <- paper.df[,1:(fold.line - 1)]
  right <- paper.df[,(fold.line + 1):ncol(paper.df)]
  flipped.right <- right[,ncol(right):1]
  paper.df <- left + flipped.right
  return(paper.df)
}

# first fold
if(fold.df[1,1][[1]] == 'y'){
  paper.df <- fold.up(paper.df,fold.df[1,2][[1]])
} else paper.df <- fold.left(paper.df,fold.df[1,2][[1]])

paper.df[paper.df > 0] <- 1
sum(paper.df)
# 729


# part 2 ------------------------------------------------------------------

# continue folding
for (i in 2:nrow(fold.df)){
  if(fold.df[i,1][[1]] == 'y'){
    paper.df <- fold.up(paper.df,fold.df[i,2][[1]])
  } else paper.df <- fold.left(paper.df,fold.df[i,2][[1]])
}


paper.df[paper.df > 0] <- 1
paper.df <- paper.df %>% mutate_all(as.character)

paper.df[paper.df == '0'] <- '.'
paper.df[paper.df == '1'] <- '#'

print(paper.df)  
# RGZLBHFP  
  