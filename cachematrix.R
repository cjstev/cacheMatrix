## Programming Assignment #2 by CJ

## makeCacheMatrix creates a list with four separate functions
## set() allows the user to create a matrix
## get() retrieves the stored matrix and prints
## setInverse() allows the user (or other function) to set the inverse
## getInverse() allows the inverse to be printed from the cache

makeCacheMatrix <- function(x = numeric()) {
        
        myMatrixInverse <- NULL
        
        set <- function(y){
                x<<-y
                myMatrixInverse <- NULL
        }
        
        get <- function() x
        
        setInverse <- function(myInverseCache) myMatrixInverse <<- myInverseCache
        
        getInverse <- function() myMatrixInverse
        
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
        
}


## cacheSolve checks to see if there is a cached solution then creates it's own 
## solution if the matrix has changed or there was no solution cached

cacheSolve <- function(x, ...) {
        myMatrixInverse <- x$getInverse()
        
        if(!is.null(myMatrixInverse)) {
                message("getting cached data")
                return(myMatrixInverse)
        }
        data <- x$get()
        myMatrixInverse <- solve(data, ...)
        x$setInverse(myMatrixInverse)
        myMatrixInverse
}
