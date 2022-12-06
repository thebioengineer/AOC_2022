
data <- readLines(here::here("data/day5.txt"))

example <- c(
  "move 1 from 2 to 1",
  "move 3 from 1 to 3",
  "move 2 from 2 to 1",
  "move 1 from 1 to 2"
)

ex_start_status <- list(
  c("Z","N"),
  c("M","C","D"),
  c("P")
)


## Answer #1 = which crates are on top
move_crate <- function(state, from, to, n){

  idx_to_pull <- seq(length(state[[from]]),length(state[[from]])-n+1)
  idx_to_keep <- 1:(length(state[[from]])-n)

  new_from_crate_stack <- state[[from]][idx_to_keep]
  new_to_crate_stack <- c(state[[to]], state[[from]][idx_to_pull])

  state[[from]] <- new_from_crate_stack
  state[[to]] <- new_to_crate_stack

  state
}

parse_instructions <- function(x, state, move_crate_func){
  numbers <- as.numeric(strsplit(gsub("move (\\d+) from (\\d) to (\\d)", "\\1,\\2,\\3",x),",")[[1]])
  state <- move_crate_func(state, from = numbers[[2]], to = numbers[[3]], n = numbers[[1]])
  state
}

top_of_stacks <- function(state){
  crates <- c()

  for(stack in state){
    crates <- c(crates, stack[length(stack)])
  }

  paste0(crates, collapse = "")
}

ex_status <- ex_start_status

for(i in example){
  ex_status <- parse_instructions(i, ex_status, move_crate)
}

top_of_stacks(ex_status)

data_start_status <- list(
  c("N","B","D","T","V","G","Z","J"),
  c("S","R","M","D","W","P","F"),
  c("V","C","R","S","Z"),
  c("R","T","J","Z","P","H","G"),
  c("T","C","J","N","D","Z","Q","F"),
  c("N","V","P","W","G","S","F","M"),
  c("G","C","V","B","P","Q"),
  c("Z","B","N","P"),
  c("W","P","J")
)


steps <- data[11:length(data)]

stack_status <- data_start_status
for(i in steps){
  stack_status <- parse_instructions(i, stack_status, move_crate)
}
top_of_stacks(stack_status)


## answer 2
move_crate_2 <- function(state, from, to, n){

  idx_to_pull <- seq(length(state[[from]])-n+1,length(state[[from]]))
  idx_to_keep <- 1:(length(state[[from]])-n)

  new_from_crate_stack <- state[[from]][idx_to_keep]
  new_to_crate_stack <- c(state[[to]], state[[from]][idx_to_pull])

  state[[from]] <- new_from_crate_stack
  state[[to]] <- new_to_crate_stack

  state
}

ex_status <- ex_start_status

for(i in example){
  ex_status <- parse_instructions(i, ex_status, move_crate_2)
}

top_of_stacks(ex_status)

steps <- data[11:length(data)]

stack_status <- data_start_status
for(i in steps){

  stack_status <- parse_instructions(i, stack_status, move_crate_2)
  print(stack_status)
}
top_of_stacks(stack_status)
