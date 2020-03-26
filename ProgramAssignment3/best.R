## Course 2 Week 4 Assignment - Hospital Data

setwd("C:/Users/Lam/Desktop/coursera/ProgramAssignment3")
library(tidyverse)

best = function(state, outcome){
      full = read.csv("outcome-of-care-measures.csv")
      compact = full[,c(2,7,11,17,23)]
      compact =  compact %>% rename(
          MRHA = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
          MRHF = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
          MRP = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      compact = compact %>% mutate(Hospital.Name = as.character(Hospital.Name))
      levState = c(levels(compact$State))
      levOutcomes = c("heart attack","heart failure","pneumonia")
      validState = sum(state == levState)
      validOutcome = sum(outcome == levOutcomes)
          if(validState == 0){
                stop("invalid state")
          }
          if(validOutcome == 0){
                stop("invalid outcome")
          }
      if(outcome == "heart attack"){
          minimum = min(tapply(compact$MRHA, compact$State, min))
      }
}

