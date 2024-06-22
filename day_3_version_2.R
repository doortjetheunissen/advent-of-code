# part 1 ------------------------------------------------------------------

library(tidyverse)

df <- read.csv('day_3_input.csv', header = FALSE, colClasses = c('character'), col.names = c('binary_number')) %>%
  mutate(x = substr(strsplit(binary_number,''),0,1),
         binary_number = NULL) %>%
  unnest_wider(x) %>% 
  mutate_all(as.numeric) %>%
  summarise_all(sum)

gamma <- df ; epsilon <- df

gamma[gamma < 500] <- 0
gamma[gamma >= 500] <- 1
gamma <- paste(gamma[1,], collapse = '')

epsilon[epsilon < 500] <- 1
epsilon[epsilon >= 500] <- 0
epsilon <- paste(epsilon[1,], collapse = '')

strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)
# 738234
