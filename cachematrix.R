## cdmuhs
## Coursera: R Programming assignment 2 Lexical Scoping
###############################################################################
## These functions cache the inverse of a matrix and then 
## computes the inverse of the special "matrix."


# 1. cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)    
}

# 2. compute inverse of the special "matrix" and return the value
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    return(m)
}

# Testing
mymat = matrix(rnorm(4), 2, 2)
# mymat
mytest = makeCacheMatrix(mymat)
mytest$get()
mytest$getinv()
cacheSolve(mytest)