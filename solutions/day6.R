
data <- readLines(here::here("data/day6.txt"))

example <- c(
  "bvwbjplbgvbhsrlpgdmjqwftvncz",
  "nppdvjthqldpwncqszvftbrmjlhg",
  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
)


## answer 1 - 4 unique

all_unique <- function(x){
  length(unique(x)) == length(x)
}

id_marker_start <- function(x){
  sep_packet <- strsplit(x,"")[[1]]
  start_idx <- 0
  for(i in seq(4, length(sep_packet))){
    if(all_unique(sep_packet[seq(i-3,i)])){
      start_idx <- i
      break
    }
  }
  start_idx
}

for(i in example){
  print(id_marker_start(i))
}

id_marker_start(data)

## answer 2

id_message_marker <- function(x, marker_len = 14){
  sep_packet <- strsplit(x,"")[[1]]
  start_idx <- 0
  for(i in seq(marker_len, length(sep_packet))){
    if(all_unique(sep_packet[seq(i-(marker_len-1),i)])){
      start_idx <- i
      break
    }
  }
  start_idx
}

example2 <- c(
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
  "bvwbjplbgvbhsrlpgdmjqwftvncz",
  "nppdvjthqldpwncqszvftbrmjlhg",
  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
)


for(i in example2){
  print(id_message_marker(i))
}

id_message_marker(data)
