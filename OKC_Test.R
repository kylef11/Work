# OKC Coding Test
# Kyle Felder
# Done in RStudio

# I interpreted C3 as corner 3s & NC3 as 3s not in the corner
# I gave answers in percentages with 3 decimal places

library(knitr)
library(tidyverse)
library(readr)
library(dplyr)
library(tibble)

all <- read.csv("Downloads/shots_data.csv")

TeamA <- all[1:281,]
TeamB <- all[282:560,]


TeamA_1 <- TeamA
coordinA <- TeamA_1[,2:3]
coordin_absA <- abs(coordinA) # getting coordinates

TeamB_1 <- TeamB
coordinB <- TeamB_1[,2:3]
coordin_absB <- abs(coordinB) # getting coordinates



####### Functions ########

pythag <- function(coordinates) { # pythagorean theorem formula on x & y coordinates
  x <- coordinates[1,1]
  y <- coordinates[1,2]
  distance <- sqrt(x*x + y*y)
  return(distance) }

shot_distance <- function(coordinate_set_abs) { # finding shot distance
  data <- coordinate_set_abs
  i = 1
  dist <- c()
  
  while (i <= nrow(data) ) { 
    coor <- (data[i,])
    shot_dist <- pythag(coor)
    dist <- c(dist, shot_dist)
    i = i + 1
  }
  return(dist)
}

type_shot <- function(coordinate_set_abs, shot_distance) { # determining location of shot
  dist <- shot_distance
  coordin_abs <- coordinate_set_abs
  twoPt <- c()
  C3 <- c()
  NC3 <- c()
  j = 1
  
  for (i in 1:nrow(coordin_abs)) {
    if (!(dist[j] > 23.75) & (!(coordin_abs[i,1] > 22 & coordin_abs[i,2] <= 7.8))) {
      twoPt <- c(twoPt, i)
    }
    if ((dist[j] > 23.75) & (!(coordin_abs[i,1] > 22 & coordin_abs[i,2] <= 7.8))) {
      NC3 <- c(NC3, i)
    }
    if (coordin_abs[i,1] > 22 & coordin_abs[i,2] <= 7.8) {
      C3 <- c(C3, i) 
    }
    j = j + 1
  }
  output <- list(twoPt, NC3, C3)
  return(output)
}

eFG <- function(results) {  # counts number of made shots for a given list of results
  count = 0
  for (i in results) {
    if (i == 1) {
      count = count + 1
    }
    i = i + 1 
  }
  return(count)
}

result_indexA <- function(indexes) {  # uses a list of indexes to extract certain elements from a larger list
  location_results <- c()
  for (i in indexes) {
    location_results <- c(location_results, shot_resultA[i,])
  }
  return(location_results)
}

result_indexB <- function(indexes) {  # uses a list of indexes to extract certain elements from a larger list
  location_results <- c()
  for (i in indexes) {
    location_results <- c(location_results, shot_resultB[i,])
  }
  return(location_results)
}


###### Analysis ########

### Team A ###

distA <- shot_distance(coordin_absA)
outputA <- type_shot(coordin_absA, distA)

## Percentage of shots taken by area

# 2PT
lengths(outputA[1])/(nrow(TeamA)) * 100 # 60.85409

# NC3
lengths(outputA[2])/(nrow(TeamA)) * 100 # 32.02847

# C3
lengths(outputA[3])/(nrow(TeamA)) * 100 # 7.117438

## eFG% 

shot_resultA <- TeamA[4]
twoPtA <- outputA[1]
NC3A <- outputA[2]
C3A <- outputA[3]

# 2pt shots
twoPt_onlyA <- result_indexA(twoPtA)
twoPt_countA <- eFG(twoPt_onlyA)
twoPt_countA/(lengths(outputA[1]))*100 # 39.76608 %

# NC3 shots
NC3_onlyA <- result_indexA(NC3A)
NC3_countA <- eFG(NC3_onlyA)*(1.5)
NC3_countA/(lengths(outputA[2]))*100 # 48.33333 %

# C3 shots
C3_onlyA <- result_indexA(C3A)
C3_countA <- eFG(C3_onlyA)*(1.5)
C3_countA/(lengths(outputA[3]))*100 # 60.0 %



### TeamB ###

distB <- shot_distance(coordin_absB)
outputB <- type_shot(coordin_absB, distB)

## Percentage of shots taken by area

# 2PT
lengths(outputB[1])/(nrow(TeamB)) * 100 # 58.06452 %

# NC3
lengths(outputB[2])/(nrow(TeamB)) * 100 # 34.4086 %

# C3
lengths(outputB[3])/(nrow(TeamB)) * 100 # 7.526882 %

## eFG% 

shot_resultB <- TeamB[4]
twoPtB <- outputB[1]
NC3B <- outputB[2]
C3B <- outputB[3]

# 2pt shots
twoPt_onlyB <- result_indexB(twoPtB)
twoPt_countB <- eFG(twoPt_onlyB)
twoPt_countB/(lengths(outputB[1]))*100 # 46.2963 %

# NC3 shots
NC3_onlyB <- result_indexB(NC3B)
NC3_countB <- eFG(NC3_onlyB)*(1.5)
NC3_countB/(lengths(outputB[2]))*100 # 54.6875 %

# C3 shots
C3_onlyB <- result_indexB(C3B)
C3_countB <- (eFG(C3_onlyB))*(1.5)
C3_countB/(lengths(outputB[3]))*100 # 35.71429 %
