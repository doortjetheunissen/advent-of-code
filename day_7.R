# part 1 ------------------------------------------------------------------

library(dplyr)
library(magrittr)

df <- data.frame(t(read.csv('day_7_input.csv', header = FALSE)), row.names = NULL) %>% set_colnames(c('position'))

df <- df %>% mutate(fuel = abs(position - median(position)))

sum(df$fuel)
# 348996


# part 2 ------------------------------------------------------------------

floor.mean <- floor(mean(df$position))
ceiling.mean <- ceiling(mean(df$position))

calculate_sum <- function(n){
  if (n <= 1){
    return(n)
  } else {
    return(n + calculate_sum(n-1))
  }
}

df <- df %>% rowwise %>% mutate(rec_fuel_floor = calculate_sum(abs(position - floor.mean)), rec_fuel_ceiling = calculate_sum(abs(position - ceiling.mean)))

min(colSums(df)['rec_fuel_floor'],colSums(df)['rec_fuel_ceiling'])
# 98231647
