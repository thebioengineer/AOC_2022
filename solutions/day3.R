library(tidyverse)

data <- readLines(here::here("data/day3.txt"))

## Answer 1 - sum of priorities

split_rucksack <- function(x){
  tot_items <- nchar(x)
  split_on <- floor(tot_items/2)
  ruck_1 <- substr(x, 0, split_on)
  ruck_2 <- substr(x, split_on + 1, tot_items)
  c(
    ruck_1,
    ruck_2
  )
}

find_dupes <- function(x, y){
  x_split <- str_split(x,"")[[1]]
  y_split <- str_split(y,"")[[1]]
  intersect(x_split, y_split)
}

priority <- setNames(c(1:52),c(letters,LETTERS))

calc_priority <- function(x){
  priority[x]
}

### example

ex <- c(
  "vJrwpWtwJgWrhcsFMMfFFhFp",
  "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
  "PmmdzqPrVvPwwTWBwg",
  "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
  "ttgJtRGJQctTZtZT",
  "CrZsJsPPZsGzwwsLwLmpwMDw"
)

ex_tot_priorities <- 0
for(i in ex){
  rucksacks <- split_rucksack(i)
  dupes <- find_dupes(rucksacks[1],rucksacks[2])
  ex_tot_priorities <- ex_tot_priorities + calc_priority(dupes)
}
ex_tot_priorities

# actual
tot_priorities <- 0
for(i in data){
  rucksacks <- split_rucksack(i)
  dupes <- find_dupes(rucksacks[1],rucksacks[2])
  tot_priorities <- tot_priorities + calc_priority(dupes)
}
tot_priorities

## Answer 2 - sum of priorities by groups of 3

find_dupes_across_3 <- function(x, y, z){
  x_split <- str_split(x,"")[[1]]
  y_split <- str_split(y,"")[[1]]
  z_split <- str_split(z,"")[[1]]
  intersect(intersect(x_split, y_split),z_split)
}

ex_groups <- seq(1,length(ex),by = 3)
ex_tot_priorities <- 0
for(i in ex_groups){
  dupes <- find_dupes_across_3(ex[i],ex[i+1],ex[i+2])
  ex_tot_priorities <- ex_tot_priorities + calc_priority(dupes)
}
ex_tot_priorities

# actual

groups <- seq(1,length(data),by = 3)

tot_priorities <- 0
for(i in groups){
  dupes <- find_dupes_across_3(data[i],data[i+1],data[i+2])
  tot_priorities <- tot_priorities + calc_priority(dupes)
}
tot_priorities
