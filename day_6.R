# part 1 ------------------------------------------------------------------

library(dplyr)
library(magrittr)
options(scipen = 999)

df <- data.frame(t(read.csv('day_6_input.csv', header = FALSE)) %>% set_colnames(c('fish_timer')))
rownames(df) <- NULL

df <- df %>% group_by(fish_timer) %>% summarise(fish_count = n())
timer.list <- data.frame(c(0,1,2,3,4,5,6,7,8)) %>% set_colnames(c('fish_timer'))

df.count <- left_join(timer.list,df)
df.count[is.na(df.count)] <- 0

df.temp <- df.count
df.temp$fish_count <- 0

for (i in 1:80){
  df.temp[df.temp$fish_timer == 0,'fish_count'] <- df.count[df.count$fish_timer == 1,'fish_count']
  df.temp[df.temp$fish_timer == 1,'fish_count'] <- df.count[df.count$fish_timer == 2,'fish_count']
  df.temp[df.temp$fish_timer == 2,'fish_count'] <- df.count[df.count$fish_timer == 3,'fish_count']
  df.temp[df.temp$fish_timer == 3,'fish_count'] <- df.count[df.count$fish_timer == 4,'fish_count']
  df.temp[df.temp$fish_timer == 4,'fish_count'] <- df.count[df.count$fish_timer == 5,'fish_count']
  df.temp[df.temp$fish_timer == 5,'fish_count'] <- df.count[df.count$fish_timer == 6,'fish_count']
  df.temp[df.temp$fish_timer == 6,'fish_count'] <- df.count[df.count$fish_timer == 7,'fish_count'] +
    df.count[df.count$fish_timer == 0,'fish_count']
  df.temp[df.temp$fish_timer == 7,'fish_count'] <- df.count[df.count$fish_timer == 8,'fish_count']
  df.temp[df.temp$fish_timer == 8,'fish_count'] <- df.count[df.count$fish_timer == 0,'fish_count']
  df.count <- df.temp
  df.temp$fish_count <- 0
}

sum(df.count$fish_count)
# 386755


# part 2 ------------------------------------------------------------------

df.count <- left_join(timer.list,df)
df.count[is.na(df.count)] <- 0

df.temp$fish_count <- 0

for (i in 1:256){
  df.temp[df.temp$fish_timer == 0,'fish_count'] <- df.count[df.count$fish_timer == 1,'fish_count']
  df.temp[df.temp$fish_timer == 1,'fish_count'] <- df.count[df.count$fish_timer == 2,'fish_count']
  df.temp[df.temp$fish_timer == 2,'fish_count'] <- df.count[df.count$fish_timer == 3,'fish_count']
  df.temp[df.temp$fish_timer == 3,'fish_count'] <- df.count[df.count$fish_timer == 4,'fish_count']
  df.temp[df.temp$fish_timer == 4,'fish_count'] <- df.count[df.count$fish_timer == 5,'fish_count']
  df.temp[df.temp$fish_timer == 5,'fish_count'] <- df.count[df.count$fish_timer == 6,'fish_count']
  df.temp[df.temp$fish_timer == 6,'fish_count'] <- df.count[df.count$fish_timer == 7,'fish_count'] +
    df.count[df.count$fish_timer == 0,'fish_count']
  df.temp[df.temp$fish_timer == 7,'fish_count'] <- df.count[df.count$fish_timer == 8,'fish_count']
  df.temp[df.temp$fish_timer == 8,'fish_count'] <- df.count[df.count$fish_timer == 0,'fish_count']
  df.count <- df.temp
  df.temp$fish_count <- 0
}

sum(df.count$fish_count)
# 1732731810807
