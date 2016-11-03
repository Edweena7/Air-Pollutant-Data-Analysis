rankall<-function(disease,n){
    if(!(disease %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
    a<-0 
    c<-0
    if(n=="best") nn=1
    data<-matrix(ncol=2,nrow=54)
    statename <- unique(outcome$State)
    if(disease=="heart attack"){
    
        for(i in statename){
            a<-a+1
            h <- filter(outcome,State==i)
            h <- select(h,c(2,11))
            h[,2] <- as.numeric(h[,2])
            hospital<-h[order(h$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,h$Hospital.Name,na.last=NA),]
            if(n=="worst") nn=nrow(hospital)
            else nn=n
            data[a,]<-c(hospital[nn,1],i)
         }
    }
    if(disease=="heart failure"){
        for(i in statename){
            a<-a+1
            h <- filter(outcome,State==i)
            h <- select(h,c(2,17))
            h[,2] <- as.numeric(h[,2])
            hospital<-h[order(h$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,h$Hospital.Name,na.last=NA),]
            if(n=="worst") nn=nrow(hospital)
            else nn=n
            data[a,]<-c(hospital[nn,1],i)
        }
    }
    if(disease=="pneumonia"){
        for(i in statename){
            a<-a+1
            h <- filter(outcome,State==i)
            h <- select(h,c(2,23))
            h[,2] <- as.numeric(h[,2])
            hospital<-h[order(h$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,h$Hospital.Name,na.last=NA),]
            if(n=="worst") nn=nrow(hospital)
            else nn=n
            data[a,]<-c(hospital[nn,1],i)
        }
    }
    
    data<-data.frame(data)
    colnames(data)<-c("hospital","state")
    data
    
}
