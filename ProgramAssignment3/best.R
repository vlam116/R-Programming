## Course 2 Week 4 Assignment - Hospital Data
setwd("C:/Users/Lam/Desktop/coursera/datasciencecoursera/ProgramAssignment3")

library(tidyverse)

best = function(state, outcome){
      full = read.csv("outcome-of-care-measures.csv")
      compact = full[,c(2,7,11,17,23)]
      compact =  compact %>% rename(
          MRHA = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
          MRHF = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
          MRP = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      compact = compact %>% mutate(Hospital.Name = as.character(Hospital.Name),
                                   MRHA = as.numeric(MRHA),
                                   MRHF = as.numeric(MRHF),
                                   MRP = as.numeric(MRP))
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
          subsetHA = compact[compact$State == state,]
          minHA = min(subsetHA$MRHA)
          bestHospitalHA = subsetHA[which(subsetHA$MRHA==minHA),]
          hospitalNameHA = bestHospitalHA$Hospital.Name
          tiedHA = sort(hospitalNameHA)
          print(tiedHA)
      }
      if(outcome == "heart failure"){
         subsetHF = compact[compact$State == state,]
         minHF = min(subsetHF$MRHF)
         bestHospitalHF = subsetHF[which(subsetHF$MRHF==minHF),]
         hospitalNameHF = bestHospitalHF$Hospital.Name
         tiedHF = sort(hospitalNameHF)
         print(tiedHF)
      }
}

