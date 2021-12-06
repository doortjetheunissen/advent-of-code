# part 1 ------------------------------------------------------------------

library(stringr)

df <- read.csv('day_4_input.csv', header = FALSE)
draw.list <- as.numeric(df[1,])

df <- df[2:501,]
board.list <- list()

for (i in 0:(nrow(df)/5-1)){
  a <- as.numeric(str_split(df[(i*5)+1,1], ' ')[[1]])
  b <- as.numeric(str_split(df[(i*5)+2,1], ' ')[[1]])
  c <- as.numeric(str_split(df[(i*5)+3,1], ' ')[[1]])
  d <- as.numeric(str_split(df[(i*5)+4,1], ' ')[[1]])
  e <- as.numeric(str_split(df[(i*5)+5,1], ' ')[[1]])
  board <- list(data.frame(rbind(a,b,c,d,e)))
  board.list <- c(board.list, board)
}

board.list.backup <- board.list

win <- 0
winning.board <- 0
draw.position <- 0

while (win == 0){
  draw.position <- draw.position + 1
  last.draw <- draw.list[draw.position]
  for (i in 1:length(board.list)){
    board.list[[i]][board.list[[i]] == last.draw] <- 1000
    col.check <- colSums(board.list[[i]])
    row.check <- rowSums(board.list[[i]])
    if (5000 %in% col.check | 5000 %in% row.check){
      win <- 1
      winning.board <- i
    }
  }
}

sum(board.list[[winning.board]][board.list[[winning.board]] != 1000]) * last.draw
#8442


# part 2 ------------------------------------------------------------------

sum.order <- as.numeric()
board.list <- board.list.backup
draw.position <- 0

while(length(sum.order) < 100){
  draw.position <- draw.position + 1
  last.draw <- draw.list[draw.position]
  for (i in 1:length(board.list)){
    board.list[[i]][board.list[[i]] == last.draw] <- 1000
    col.check <- colSums(board.list[[i]])
    row.check <- rowSums(board.list[[i]])
    if (5000 %in% col.check | 5000 %in% row.check){
      board.sum <- sum(board.list[[i]][board.list[[i]] != 1000])
      sum.order <- c(sum.order,board.sum)
      board.list[[i]] <- data.frame(matrix(NA, nrow = 5, ncol = 5))
    }
  }
}

sum.order[100] * last.draw
#4590

