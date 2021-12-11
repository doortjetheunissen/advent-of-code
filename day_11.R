# input manipulation
# find ([0-9])
# replace ,\1

# part 1 ------------------------------------------------------------------
library(dplyr)
library(magrittr)

df <- read.csv('day_11_input.csv', header = FALSE)[,2:11] %>% set_colnames(1:10)

flash.count <- 0

check.for.flash <-function(df,to.flash,has.flashed){
  to.flash <- rbind(to.flash, which(df > 9, arr.ind = TRUE))
  if(nrow(setdiff(to.flash,has.flashed)) > 0){
    row.pos <- setdiff(to.flash,has.flashed)[1,1]
    col.pos <- setdiff(to.flash,has.flashed)[1,2]
    df <- do.flash(df,row.pos,col.pos)
    flash.count <<- flash.count + 1
    has.flashed <- rbind(has.flashed,setdiff(to.flash,has.flashed)[1,])
    df <- check.for.flash(df,to.flash,has.flashed)
  } else if (nrow(has.flashed) > 1){
    for (j in 2:nrow(has.flashed)){
      df[has.flashed[j,1],has.flashed[j,2]] <- 0
    }
    return(df)
  } else return(df)
}

do.flash <- function(df,row.pos,col.pos){
  if (row.pos > 1 & col.pos > 1) df[row.pos-1,col.pos-1] %<>% +1
  if (row.pos > 1) df[row.pos-1,col.pos] %<>% +1
  if (row.pos > 1 & col.pos < 10) df[row.pos-1,col.pos+1] %<>% +1
  if (col.pos > 1) df[row.pos,col.pos-1] %<>% +1
  if (col.pos < 10) df[row.pos,col.pos+1] %<>% +1
  if (row.pos < 10 & col.pos > 1) df[row.pos+1,col.pos-1] %<>% +1
  if (row.pos < 10) df[row.pos+1,col.pos] %<>% +1
  if (row.pos < 10 & col.pos < 10) df[row.pos+1,col.pos+1] %<>% +1
  return(df)
}


for (i in 1:100){
  df <- df + 1
  to.flash <- data.frame(matrix(c(0,0), ncol = 2)) %>% set_colnames(c('row','col'))
  has.flashed <- data.frame(matrix(c(0,0), ncol = 2)) %>% set_colnames(c('row','col'))
  df <- check.for.flash(df,to.flash,has.flashed)
}

flash.count
# 1625


# part 2 ------------------------------------------------------------------
#rewrite of part 1

library(dplyr)
library(magrittr)

df <- read.csv('day_11_input.csv', header = FALSE)[,2:11] %>% set_colnames(1:10)

check.for.flash <-function(df,to.flash,has.flashed){
  to.flash <- rbind(to.flash, which(df > 9, arr.ind = TRUE))
  if(nrow(setdiff(to.flash,has.flashed)) > 0){
    row.pos <- setdiff(to.flash,has.flashed)[1,1]
    col.pos <- setdiff(to.flash,has.flashed)[1,2]
    df <- do.flash(df,row.pos,col.pos)
    has.flashed <- rbind(has.flashed,setdiff(to.flash,has.flashed)[1,])
    df <- check.for.flash(df,to.flash,has.flashed)
  } else if (nrow(has.flashed) > 1){
    for (j in 2:nrow(has.flashed)){
      df[has.flashed[j,1],has.flashed[j,2]] <- 0
    }
    return(df)
  } else return(df)
}

do.flash <- function(df,row.pos,col.pos){
  if (row.pos > 1 & col.pos > 1) df[row.pos-1,col.pos-1] %<>% +1
  if (row.pos > 1) df[row.pos-1,col.pos] %<>% +1
  if (row.pos > 1 & col.pos < 10) df[row.pos-1,col.pos+1] %<>% +1
  if (col.pos > 1) df[row.pos,col.pos-1] %<>% +1
  if (col.pos < 10) df[row.pos,col.pos+1] %<>% +1
  if (row.pos < 10 & col.pos > 1) df[row.pos+1,col.pos-1] %<>% +1
  if (row.pos < 10) df[row.pos+1,col.pos] %<>% +1
  if (row.pos < 10 & col.pos < 10) df[row.pos+1,col.pos+1] %<>% +1
  return(df)
}

all.in.sync <- 0
step <- 0

while(all.in.sync == 0){
  step <- step + 1
  df <- df + 1
  to.flash <- data.frame(matrix(c(0,0), ncol = 2)) %>% set_colnames(c('row','col'))
  has.flashed <- data.frame(matrix(c(0,0), ncol = 2)) %>% set_colnames(c('row','col'))
  df <- check.for.flash(df,to.flash,has.flashed)
  if (nrow(which(df == 0, arr.ind = TRUE)) == 100){
    all.in.sync <- 1
  }
}

step
# 244
