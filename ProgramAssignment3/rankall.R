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
    compactHA = compact[,c(1,2,3)]
    compactHF = compact[,c(1,2,4)]
    compactP = compact[,c(1,2,5)]
    compactHA = na.omit(compactHA)
    compactHF = na.omit(compactHF)
    compactP = na.omit(compactP)
    
    if(outcome == "heart attack"){
      spl = split(compactHA, compactHA$State)
      sorted = spl %>% lapply(arrange, MRHA, Hospital.Name)
      hNames = lapply(sorted, function(x) x$Hospital.Name[num])
      sNames = lapply(sorted, function(x) x$State[1])
      hospital = c(hNames)
      state = c(sNames)
      all = cbind(hospital, state)
    }
    
    if(outcome == "heart failure"){
      spl = split(compactHF, compactHF$State)
      sorted = spl %>% lapply(arrange, MRHF, Hospital.Name)
      hNames = lapply(sorted, function(x) x$Hospital.Name[num])
      sNames = lapply(sorted, function(x) x$State[1])
      hospital = c(hNames)
      state = c(sNames)
      all = cbind(hospital, state)
    }
    
    if(outcome == "pneumonia"){
      spl = split(compactP, compactP$State)
      sorted = spl %>% lapply(arrange, MRP, Hospital.Name)
      hNames = lapply(sorted, function(x) x$Hospital.Name[num])
      sNames = lapply(sorted, function(x) x$State[1])
      hospital = c(hNames)
      state = c(sNames)
      all = cbind(hospital, state)
    }
    
    if(num == "best"){
      hNames = lapply(sorted, function(x) x$Hospital.Name[1])
      sNames = lapply(sorted, function(x) x$State[1])
      hospital = c(hNames)
      state = c(sNames)
      all = cbind(hospital, state)
    }
    
    if(num == "worst"){
      hNames = lapply(sorted, function(x) x$Hospital.Name[nrow(x)])
      sNames = lapply(sorted, function(x) x$State[1])
      hospital = c(hNames)
      state = c(sNames)
      all = cbind(hospital, state)
    }
  as.data.frame(all)
}