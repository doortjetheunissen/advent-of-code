# part 1 ------------------------------------------------------------------

library(stringr)
library(dplyr)
library(GA)

df <- read.csv('day_3_input.csv', header = FALSE, colClasses = c('character'), col.names = c('binary_number'))

df <- df %>% mutate(b1 = as.numeric(str_sub(binary_number, 1, 1)),
                    b2 = as.numeric(str_sub(binary_number, 2, 2)),
                    b3 = as.numeric(str_sub(binary_number, 3, 3)),
                    b4 = as.numeric(str_sub(binary_number, 4, 4)),
                    b5 = as.numeric(str_sub(binary_number, 5, 5)),
                    b6 = as.numeric(str_sub(binary_number, 6, 6)),
                    b7 = as.numeric(str_sub(binary_number, 7, 7)),
                    b8 = as.numeric(str_sub(binary_number, 8, 8)),
                    b9 = as.numeric(str_sub(binary_number, 9, 9)),
                    b10 = as.numeric(str_sub(binary_number, 10, 10)),
                    b11 = as.numeric(str_sub(binary_number, 11, 11)),
                    b12 = as.numeric(str_sub(binary_number, 12, 12)))

gamma <- ''

for (i in 1:12){
  if (sum(df[,i+1]) >= 500){
    gamma <- str_c(gamma,'1')
  } else {
    gamma <- str_c(gamma,'0')
  }
}

epsilon <- ''

for (i in 1:12){
  if (sum(df[,i+1]) >= 500){
    epsilon <- str_c(epsilon,'0')
  } else {
    epsilon <- str_c(epsilon,'1')
  }
}

gamma.vector <- as.numeric(str_split_fixed(gamma,pattern = '', n = 12))
epsilon.vector <- as.numeric(str_split_fixed(epsilon,pattern = '', n = 12))

binary2decimal(gamma.vector) * binary2decimal(epsilon.vector)
#738234


# part 2 ------------------------------------------------------------------

### oxygen - keep most common

df.oxygen <- df[,2:13]
i <- 1

while(nrow(df.oxygen) > 1){
  if (sum(df.oxygen[,i])/nrow(df.oxygen) >= 0.5){
    df.oxygen <- df.oxygen[df.oxygen[,i] == 1,]
  } else {
    df.oxygen <- df.oxygen[df.oxygen[,i] == 0,]
  }
  i <- i + 1
}

oxygen.vector <- as.numeric(df.oxygen)


### co2 - least common

df.co2 <- df[,2:13]
i <- 1

while(nrow(df.co2) > 1){
  if (sum(df.co2[,i])/nrow(df.co2) >= 0.5){
    df.co2 <- df.co2[df.co2[,i] == 0,]
  } else {
    df.co2 <- df.co2[df.co2[,i] == 1,]
  }
  i <- i + 1
}

co2.vector <- as.numeric(df.co2)


binary2decimal(oxygen.vector) * binary2decimal(co2.vector)
#3969126
