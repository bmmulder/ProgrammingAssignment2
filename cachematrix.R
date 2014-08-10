## The first function, makeCacheMatrix creates a special "matrix"

# setup cached matrix object
makeCacheMatrix <- function(x = matrix()) {
    # init the matrix variable
    m <- NULL

    # set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    # set the value of the inverted matrix
    setinverse <- function(solve) m <<- solve
    # get the value of the inverted matrix
    getinverse <- function() m
    
    # add accessor list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverted special "matrix" 
## created with the above function. 

cacheSolve <- function(x, ...) {
    # Try to get the cached matrix
    m <- x$getinverse()
    
    # if there is a cached version
    if(!is.null(m)) {
        message("getting cached data")
        # return the cached matrix
        return(m)
    }
    
    # or else, get the matrix
    data <- x$get()
    # invert the matrix
    m <- solve(data, ...)
    # cache it for the future
    x$setinverse(m)
    # return the inverted matrix
    m    
}
