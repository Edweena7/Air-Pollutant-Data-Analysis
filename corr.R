corr <- function(directory,threshold=0){
    
    allfiles <- list.files(path=directory,full.names=TRUE)
    
    rawdata <- complete(directory)
    data <- subset(rawdata,nobs>threshold)
    corr <- integer()
    
    for(i in data$id){
        monitordata <- read.csv(allfiles[i])
        corr <- c(corr,cor(monitordata$nitrate,monitordata$sulfate,use="complete.obs"))
    }
    corr
}
