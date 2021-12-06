# part 1 ------------------------------------------------------------------



df <- read.csv('day_1_input.csv', header = FALSE, col.names = c('depth'))
df$increase <- ''

for (i in 2:nrow(df)){
  if (df[i,"depth"] > df[i-1,"depth"]) {
    df[i,"increase"] <- 'yes'
  } else {
    df[i,"increase"] <- 'no'
  }
}

df$increase <- as.factor(df$increase)

summary(df)
#1215


# part 2 ------------------------------------------------------------------

df <- read.csv('day_1_input.csv', header = FALSE, col.names = c('depth'))
df$sum <- 0

for (i in 3:nrow(df)){
  df[i,"sum"] <- df[i,"depth"] + df[i-1,"depth"] + df[i-2,"depth"]
}

df$increase <- ''

for (i in 4:nrow(df)){
  if (df[i,"sum"] > df[i-1,"sum"]) {
    df[i,"increase"] <- 'yes'
  } else {
    df[i,"increase"] <- 'no'
  }
}

df$increase <- as.factor(df$increase)

summary(df)
#1150