##Return the result of the inverse of a matrix.
##If the result is not found, calculate the inverse and then save it to cache.
##makeCacheMatrix will create a matrix and save its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(get=get,setinverse=setinverse,getinverse=getinverse)
    
}


## cacheSolve will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
