best<-function(state,disease){
    if(!(state %in% outcome$State)) stop("invalid state")
    if(!(disease %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
    hospital <- filter(outcome,State==state)
    if(disease=="heart attack"){
        hospital <- select(hospital,c(2,11))
        besthospital <- hospital[which.min(hospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    }
    if(disease=="hear failure"){
        hospital <- select(hospital,c(2,17))
        besthospital <- hospital[which.min(hospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    }
    if(disease=="pneumonia"){
        hospital <- select(hospital,c(2,23))
        besthospital <- hospital[which.min(hospital$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    }
    
    if(nrow(besthospital)>1) besthospital<-besthospital[order(besthospital$Hospital.Name),]
    
    besthospital[1,1]
}

