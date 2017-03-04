## This file will cache the inverse of a matrix to save time
## in computation
## 

## makeCacheMatrix creates special "matrix" object that is 
##    a list containg a function to:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the matrix inverse
## 4) Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If a new inverse calculation 
## is not necessary, this function retrieves the inverse 
## from the cache

cacheSolve <- function(x, ...) {
    minv <- x$getinv()
    if (!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv
        ## Return a matrix that is the inverse of 'x'
}
