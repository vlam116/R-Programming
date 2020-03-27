## Course 2 Week 4 Assignment Part 3: Ranking all hospitals

rankall = function(outcome, num = "best"){
    full = read.csv("outcome-of-care-measures.csv")
    
    compact = full[,c(2,7,11,17,23)]
    compact =  compact %>% rename(
      MRHA = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
      MRHF = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
      MRP = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    compact = compact %>% mutate(State = as.character(State),
                                 Hospital.Name = as.character(Hospital.Name),
                                 MRHA = suppressWarnings(as.numeric(as.character(MRHA))),
                                 MRHF = suppressWarnings(as.numeric(as.character(MRHF))),
                                 MRP = suppressWarnings(as.numeric(as.character(MRP))))
    compact = as.tibble(compact)
    levOutcomes = c("heart attack","heart failure","pneumonia")
    validOutcome = sum(outcome == levOutcomes)
    
    if(validOutcome == 0){
      stop("invalid outcome")
    }
    
    state = c()
    hospital = c()
    if(outcome == "heart attack"){
      spl = split(compact, compact$State)
      sorted = spl %>% lapply(arrange, MRHA, Hospital.Name)
      hNames = lapply(sorted, function(x) x$Hospital.Name[num])
      sNames = lapply(sorted, function(x) x$State[1])
      hospital = c(hNames)
      state = c(sNames)
      cbind(hospital, state)
    }
}