complete <- function(directory,id = 1:332){
    
    
    allfiles <- list.files(path=directory,full.names=TRUE)
    
    data <- data.frame(id=integer(),nobs=integer())
    
    for(i in id){
        completedata <- complete.cases(read.csv(allfiles[i]))
        temp <- c(i,sum(completedata))
        data <- rbind(data,temp)
    }
    colnames(data) <- c("id","nobs")
    data
}
