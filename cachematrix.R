## The first function, makeCacheMatrix creates a special "matrix"
## containing a function to

## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverted matrix
## 4 get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverted special "matrix" 
## created with the above function. 
## It first checks to see if there is already an inverted version
## If so, it gets the inverted matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverted version sets the value of the inverted matrix
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m    
}
