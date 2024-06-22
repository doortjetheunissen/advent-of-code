library(binaryLogic)

options(scipen = 999)

# converting hex string to 4bit binary
hex_string <- '8A004A801A8002F478'
hex_string <- (hex_string %>% strsplit(''))[[1]]

binary_signal <- ''

for (i in 1:length(hex_string)){
  binary_str <- as.character(as.binary(as.numeric(strtoi(hex_string[i], base = 16L))))
  binary_str <- str_pad(paste(binary_str, collapse = ''), 4, side = 'left', pad = '0')
  binary_signal <- paste0(binary_signal, binary_str)
}

# packet version
packet_version <- strtoi(str_sub(binary_signal, 1, 3), base = 2)

# typeID
type_id <- strtoi(str_sub(binary_signal, 4, 6), base = 2)

# function - value packets
value_packet <- function(binary_signal){
  last_5 <- 0
  while (last_5 == 0){
    first_bit <- str_sub(binary_signal, 1, 1)
    four_bits <- str_sub(binary_signal, 2, 5)
  }
}

# length type id + info
if (type_id != 4){
  length_type_id <- str_sub(binary_signal, 7, 7)
  if(length_type_id == 0){
    length_of_sub <- strtoi(str_sub(binary_signal, 8, 22), base = 2)
  } else {
    number_of_sub <- strtoi(str_sub(binary_signal, 8, 18), base = 2)
  }
}

 
100 010 1 00000000001 
  001 010 1 00000000001 
    101 010 0 000000000001011
      110 100 01111 000   -> 15


