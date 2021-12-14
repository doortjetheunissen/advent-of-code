library(dplyr)
library(magrittr)
library(stringr)
options(scipen = 999)

# code for both parts -----------------------------------------------------
# get input
#file_name <- 'day_14_test.csv'
file_name <- 'day_14_input.csv'

polymer_template <- read.csv(file_name, header = FALSE)[1,]

rules.df <- data.frame(read.csv(file_name, header = FALSE)[-1,]) %>%
  set_colnames(c('rule')) %>%
  rowwise %>%
  mutate(pair = str_split(rule, pattern = ' -> ')[[1]][1],
         insert = str_split(rule, pattern = ' -> ')[[1]][2],
         rule = NULL) %>%
  mutate(result1 = str_c(str_sub(pair,1,1),insert),
         result2 = str_c(insert,str_sub(pair,2,2)),
         insert = NULL,
         pair.count = 0,
         new.count = 0)

# get pair counts for polymer template
for (i in 1:(nchar(polymer_template)-1)){
  pair <- str_sub(polymer_template, i, i+1)
  rules.df$pair.count[rules.df$pair == pair] <- rules.df$pair.count[rules.df$pair == pair] +1
}

# function - update count per step
update.count <- function(rules.df){
  for (i in 1:nrow(rules.df)){
    rules.df$new.count[rules.df$pair == rules.df$result1[i]] %<>% + rules.df$pair.count[i]
    rules.df$new.count[rules.df$pair == rules.df$result2[i]] %<>% + rules.df$pair.count[i]
  }
  rules.df$pair.count <- rules.df$new.count
  rules.df$new.count <- 0
  return(rules.df)
}


# part 1 ------------------------------------------------------------------
# run 10 steps
for (i in 1:10){
  rules.df <- update.count(rules.df)
  print(i)
}

# get elements and counts
rules.df <- rules.df %>% rowwise %>%
  mutate(first.letter = str_sub(pair,1,1)) %>%
  group_by(first.letter) %>%
  summarise(total_count = sum(pair.count))

last.letter <- str_sub(polymer_template, -1, -1)

rules.df$total_count[rules.df$first.letter == last.letter] %<>% +1 

# get max min
max(rules.df$total_count) - min(rules.df$total_count)
# 3143


# part 2 ------------------------------------------------------------------
# run 40 steps
for (i in 1:40){
  rules.df <- update.count(rules.df)
  print(i)
}

# get elements and counts
rules.df <- rules.df %>% rowwise %>%
  mutate(first.letter = str_sub(pair,1,1)) %>%
  group_by(first.letter) %>%
  summarise(total_count = sum(pair.count))

last.letter <- str_sub(polymer_template, -1, -1)

rules.df$total_count[rules.df$first.letter == last.letter] %<>% +1 

# get max min
max(rules.df$total_count) - min(rules.df$total_count)
# 4110215602456
