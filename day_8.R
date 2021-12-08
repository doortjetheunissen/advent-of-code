# part 1 ------------------------------------------------------------------

library(dplyr)
library(stringr)

df <- read.csv('day_8_input.csv', header = FALSE, col.names = c('line'))

df <- df %>% rowwise %>% mutate(output = str_split(line, pattern = '&')[[1]][2], 
                                output_str = str_split(output, pattern = ' ')) %>% select(line, output_str)

digit_count <- 0

for (i in 1:nrow(df)){
  for (j in 1:4){
    if (nchar(df$output_str[[i]][j]) %in% c(2,3,4,7)){
      digit_count <- digit_count +1
    }
  }
}

digit_count
#488


# part 2 ------------------------------------------------------------------

df <- df %>% rowwise %>% mutate(input = str_split(line, pattern = '&')[[1]][1],
                                input_str = str_split(input, pattern = ' ')) %>% select(input_str,output_str)

df <- df %>% mutate(digit_1 = '', digit_4 = '', digit_7 = '', segment_bd = '', output_num = '')

for (i in 1:nrow(df)){
  for (j in 1:10){
    if (nchar(df$input_str[[i]][j]) == 2){
      df$digit_1[i] <- str_split(df$input_str[[i]][j], '')
    } else if (nchar(df$input_str[[i]][j]) == 3) {
      df$digit_7[i] <- str_split(df$input_str[[i]][j], '')
    } else if (nchar(df$input_str[[i]][j]) == 4) {
      df$digit_4[i] <- str_split(df$input_str[[i]][j], '')
    } 
  }
  df$segment_bd[i] <- list(df$digit_4[i][[1]][!df$digit_4[i][[1]] %in% df$digit_1[i][[1]]])
}


for (i in 1:nrow(df)) {
  for (j in 1:4) {
    if (nchar(df$output_str[[i]][j]) == 2) df$output_num[i] <- str_c(df$output_num[i], 1)
    else if (nchar(df$output_str[[i]][j]) == 3) df$output_num[i] <- str_c(df$output_num[i], 7)
    else if (nchar(df$output_str[[i]][j]) == 4) df$output_num[i] <- str_c(df$output_num[i], 4)
    else if (nchar(df$output_str[[i]][j]) == 5) {
      if (sum(str_detect(df$output_str[[i]][j], df$digit_7[i][[1]])) == 3) df$output_num[i] <- str_c(df$output_num[i], 3)
      else if (sum(str_detect(df$output_str[[i]][j], df$segment_bd[i][[1]])) == 2) df$output_num[i] <- str_c(df$output_num[i], 5)
      else df$output_num[i] <- str_c(df$output_num[i], 2)
    } else if (nchar(df$output_str[[i]][j]) == 6) {
      if (sum(str_detect(df$output_str[[i]][j], df$digit_4[i][[1]])) == 4) df$output_num[i] <- str_c(df$output_num[i], 9)
      else if (sum(str_detect(df$output_str[[i]][j], df$digit_7[i][[1]])) == 3) df$output_num[i] <- str_c(df$output_num[i], 0)
      else df$output_num[i] <- str_c(df$output_num[i], 6)
    } else df$output_num[i] <- str_c(df$output_num[i], 8)
  }
}

sum(as.numeric(df$output_num))
# 1040429

