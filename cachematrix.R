makeCacheMatrix <- function(x = matrix()) {
# function that creates a special matrix object that can cache its inverse
    
        # create NULL variable m
    m <- NULL
    
    # create functions set, setinverse, getinverse
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    #define final output
    list(set=set, get = get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
# computes the inverse of the special matrix returned by makeCacheMatrix
# if the inverse has already been calculated (and the matrix has not changed)
# then the cacheSolve will retrieve the inverse from the cache
    
    # m gets assigned the getinverse part of the matrix 
    m <- x$getinverse()
    
    # checks if a cache of the inverse exists and if it does it returns the inverse from cache 
    # without recalculating
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
           
}
