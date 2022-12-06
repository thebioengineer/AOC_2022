library(tidyverse)

elf_calories <- readLines(here::here("data/day1.txt"))

n_elves <- sum(elf_calories == "") + 1

elf_level_calories <- vector("numeric", length = n_elves)

elf_idx <- 1

for(i in elf_calories){
  if(i == ""){
    elf_idx <- elf_idx + 1
  }else{
    elf_level_calories[[elf_idx]] <- elf_level_calories[[elf_idx]] + as.numeric(i)
  }
}

## answer #1 - most calories

max(elf_level_calories)

## answer #2 - top 3 elf calories sum

sum(sort(elf_level_calories, decreasing = TRUE)[1:3])
