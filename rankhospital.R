rankhospital<-function(state,disease,n){
    if(!(state %in% outcome$State)) stop("invalid state")
    if(!(disease %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
    hospital <- filter(outcome,State==state)
    if(disease=="heart attack"){
        hospital <- select(hospital,c(2,11))
        hospital[,2] <- as.numeric(hospital[,2])
  ##      hospital <- hospital[-which(is.na(hospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
        hospital<-hospital[order(hospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,hospital$Hospital.Name),]
        
    }
    if(disease=="hear failure"){
        hospital <- select(hospital,c(2,17))
        hospital[,2] <- as.numeric(hospital[,2])
  ##      hospital <- hospital[-which(is.na(hospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
        hospital<-hospital[order(hospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,hospital$Hospital.Name),]
    }
    if(disease=="pneumonia"){
        hospital <- select(hospital,c(2,23))
        hospital[,2] <- as.numeric(hospital[,2])
 ##       hospital <- hospital[-which(is.na(hospital$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
        hospital<-hospital[order(hospital$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,hospital$Hospital.Name),]
    }
    hospital<-hospital[-which(is.na(hospital[,2])),]
    
    
    if(n=="best") n=1
    
    if(n=="worst") n=nrow(hospital)
    
    
    hospital[n,1]
}
