# part 1 ------------------------------------------------------------------
library(stringr)
library(dplyr)
library(magrittr)

options(scipen = 999)

df <- read.csv('day_10_input.csv', header = FALSE)

score.df <- data.frame(c(')',']','}','>'),c(3,57,1197,25137),c(1,2,3,4)) %>% set_colnames(c('sign','corrupt_points','incomplete_points'))

closing.parts <- c(')',']','}','>') ; names(closing.parts) <- c('(','[','{','<')

corrupt <- vector()
closing.parts.needed.vc <- vector()


for (i in 1:nrow(df)){
  line.length <- nchar(df[i,1])
  closing.parts.needed.row <- vector()
  for (j in 1:line.length){
    if (str_sub(df[i,1],j,j) %in% c('(','[','{','<')) {
      closing.parts.needed.row <- c(closing.parts[str_sub(df[i,1],j,j)], closing.parts.needed.row)
    } else if (str_sub(df[i,1],j,j) == closing.parts.needed.row[1]) {
      if (length(closing.parts.needed.row) == 1) closing.parts.needed.row <- vector()
      else closing.parts.needed.row <- closing.parts.needed.row[2:length(closing.parts.needed.row)]
    } else corrupt <- c(corrupt, list(c(i,j,str_sub(df[i,1],j,j))))
  }
  closing.parts.needed.vc <- c(closing.parts.needed.vc, list(c(closing.parts.needed.row)))
}

corrupt.df <- data.frame(t(matrix(unlist(corrupt), nrow = 3))) %>% set_colnames(c('line','position','sign'))
corrupt.df$position <- as.numeric(corrupt.df$position)

corrupt.df <- corrupt.df %>% group_by(line) %>% slice(which.min(position))
corrupt.score <- left_join(corrupt.df, score.df)

sum(corrupt.score$corrupt_points)
# 216297


# part 2 ------------------------------------------------------------------
incomplete.score.vc <- vector()

for (i in 1:length(closing.parts.needed.vc)){
  if (i %in% corrupt.score$line) closing.parts.needed.vc[[i]] <- ''
  row.df <- left_join(data.frame(closing.parts.needed.vc[[i]]) %>% set_colnames(c('sign')), score.df) %>% select(incomplete_points)
  incomplete.score <- 0
  for (j in 1:nrow(row.df)){
    incomplete.score <- incomplete.score * 5
    incomplete.score <- incomplete.score + row.df[j,1]
  }
  incomplete.score.vc <- c(incomplete.score.vc, incomplete.score)
}

median(incomplete.score.vc, na.rm = TRUE)
# 2165057169
