

data <- readLines(here::here("data/day4.txt"))

example <- c(
  "2-4,6-8",
  "2-3,4-5",
  "5-7,7-9",
  "2-8,3-7",
  "6-6,4-6",
  "2-6,4-8"
)

# Answer 1 - fully contain the other?

check_full_containment <- function(x, y){

  x_in_y <- x %in% y
  y_in_x <- y %in% x

  if(all(y_in_x) | all(x_in_y)){
    return(1)
  }else{
    return(0)
  }
}

make_assignment_vecs <- function(x){
  assignments <- strsplit(x, ",")[[1]]
  assignment_1_num <- strsplit(assignments[[1]], "-")[[1]]
  assignment_1 <- seq(as.numeric(assignment_1_num[[1]]), as.numeric(assignment_1_num[[2]]))
  assignment_2_num <- strsplit(assignments[[2]], "-")[[1]]
  assignment_2 <- seq(as.numeric(assignment_2_num[[1]]), as.numeric(assignment_2_num[[2]]))
  list(
    assignment_1,
    assignment_2
  )
}

## check example
sapply(example, function(x){
  assignment <- make_assignment_vecs(x)
  check_full_containment(assignment[[1]],assignment[[2]])
}) |> sum()

## actual
sapply(data, function(x){
  assignment <- make_assignment_vecs(x)
  check_full_containment(assignment[[1]],assignment[[2]])
}) |> sum()

# Answer 2 - check Any overlap

check_any_overlap <- function(x, y){
  if(any(x %in% y)){
    return(1)
  }else{
    return(0)
  }
}

## check example
sapply(example, function(x){
  assignment <- make_assignment_vecs(x)
  check_any_overlap(assignment[[1]],assignment[[2]])
}) |> sum()

## actual
sapply(data, function(x){
  assignment <- make_assignment_vecs(x)
  check_any_overlap(assignment[[1]],assignment[[2]])
}) |> sum()
