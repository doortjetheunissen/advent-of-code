# part 1 ------------------------------------------------------------------

library(dplyr)

df <- read.csv('day_1_input.csv', header = FALSE, col.names = 'depth') %>%
  mutate(deeper = depth > lag(depth))

sum(df$deeper, na.rm = TRUE)
# 1215



# part 2 ------------------------------------------------------------------

df <- read.csv('day_1_input.csv', header = FALSE, col.names = 'depth') %>%
  mutate(lead1 = lead(depth),
         lead2 = lead(depth,2),
         sum = depth + lead1 + lead2,
         deeper = sum > lag(sum))

sum(df$deeper, na.rm = TRUE)
# 1150