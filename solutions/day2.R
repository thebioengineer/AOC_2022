library(tidyverse)

data <- readLines(here::here("data/day2.txt")) %>%
  strsplit(" ")



## answer #1 - points off of strategy guide

#keys

opp_rpc <- c(A = "Rock", B = "Paper", C = "Scissors")
your_rpc <- c(X = "Rock", Y = "Paper", Z = "Scissors")
## calc wins/round
win_pts <- function(opp, you){

  opp <- opp_rpc[opp]
  you <- your_rpc[you]

  shape_pts <- c("Rock" = 1, "Paper" = 2, "Scissors" = 3)[you]

  ## assume lose
  res_pts <- 0

  ## if you tie
  if(opp == you){
    res_pts <- 3
  }
  ## if you win
  if ((you == "Rock" & opp == "Scissors") |
      (you == "Paper" & opp == "Rock") |
      (you == "Scissors" & opp == "Paper")) {
    res_pts <- 6
  }

  tot_pts <- shape_pts + res_pts

  tot_pts
}

## example

example <- list(
  c("A","Y"),
  c("B","X"),
  c("C","Z")
)

example_tot_score <- 0
for(i in example){
  example_tot_score <- example_tot_score + win_pts(i[[1]], i[[2]])
}


tot_score <- 0
for(i in data){
  tot_score <- tot_score + win_pts(i[[1]], i[[2]])
}

tot_score

## answer #2 - wins off of strategy guide

#keys
your_goal <- c(X = "Lose", Y = "Draw", Z = "Win")

## calc wins/round
strat_pts <- function(opp, you){

  opp <- opp_rpc[opp]
  res_goal <- your_goal[you]
  res_pts <- c("Lose" = 0, "Draw" = 3, "Win" = 6)[res_goal]

  if(res_goal == "Lose"){
    if(opp == "Scissors"){
      shape <- "Paper"
    }else if (opp == "Paper"){
      shape <- "Rock"
    }else if (opp == "Rock"){
      shape <- "Scissors"
    }
  } else if (res_goal == "Win"){
    if(opp == "Scissors"){
      shape <- "Rock"
    }else if (opp == "Paper"){
      shape <- "Scissors"
    }else if (opp == "Rock"){
      shape <- "Paper"
    }
  }else if (res_goal == "Draw"){
    shape <- opp
  }

  shape_pts <- c("Rock" = 1, "Paper" = 2, "Scissors" = 3)[shape]

  tot_pts <- shape_pts + res_pts

  tot_pts
}

## example

example <- list(
  c("A","Y"),
  c("B","X"),
  c("C","Z")
)

example_tot_score <- 0
for(i in example){
  example_tot_score <- example_tot_score + strat_pts(i[[1]], i[[2]])
}


tot_score <- 0
for(i in data){
  tot_score <- tot_score + strat_pts(i[[1]], i[[2]])
}

tot_score
