## Course 2 Week 4 Assignment Part 2: Hospital Rankings

rankhospital = function(state, outcome, num = "best"){
    full = read.csv("outcome-of-care-measures.csv")
    
    compact = full[,c(2,7,11,17,23)]
       compact =  compact %>% rename(
            MRHA = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
            MRHF = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
            MRP = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
       compact = compact %>% mutate(Hospital.Name = as.character(Hospital.Name),
                               MRHA = suppressWarnings(as.numeric(as.character(MRHA))),
                               MRHF = suppressWarnings(as.numeric(as.character(MRHF))),
                               MRP = suppressWarnings(as.numeric(as.character(MRP))))
       compact = as.tibble(compact)
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
      filtered = compact %>% filter(State == state) %>% arrange(MRHA, Hospital.Name)
      filtered = filtered[,c(1,2,3)]
      filtered$Rank = 1:nrow(filtered)
      filtered = na.omit(filtered)
      rank = filtered$Hospital.Name[num]
    }
  
    if(outcome == "heart failure"){
      filtered = compact %>% filter(State == state) %>% arrange(MRHF, Hospital.Name)
      filtered = filtered[,c(1,2,4)]
      filtered$Rank = 1:nrow(filtered)
      filtered = na.omit(filtered)
      rank = filtered$Hospital.Name[num]
    }
  
    if(outcome == "pneumonia"){
      filtered = compact %>% filter(State == state) %>% arrange(MRP, Hospital.Name)
      filtered = filtered[,c(1,2,5)]
      filtered$Rank = 1:nrow(filtered)
      filtered = na.omit(filtered)
      rank = filtered$Hospital.Name[num]
    }
  
    if(num == "best"){
      rank = filtered$Hospital.Name[1]
    }
    
    if(num == "worst"){
      rank = filtered$Hospital.Name[nrow(filtered)]
    }
  rank
}