## Create a list containing functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse, and 
## get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
   
     m <<- NULL
    
## Specify the internal functions related to the matrix.     

    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
## Specify the internal functions related to the inverse. 
    
    setmatrix <- function (solve) m <<- solve
    getmatrix <- function () m
    
## Create a name list so internal functions can be retrieved by name externally
    
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Function to determine if the inverse is already in the cache.
## If it is, message "getting cached data" and returning the cached matrix.

cacheSolve <- function(x, ...) {

    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if the inverse is not already in the cache, retrieve the matrix,
    ## then apply the solve function,
    ## storing the value in m (and the cache) and printing the inverse.
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
    
}
