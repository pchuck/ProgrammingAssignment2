## coursera, programming in R, assignment2 for peer review
##   github.com/pchuck/ProgrammingAssignment2/cachematrix.R

## create a special matrix object that can cache its value and inverse
#
# this function has accessors for getting/setting a matrix and its inverse
# it uses lexical scoping to preserve state
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## return the inverse of a matrix, computed or cached
#
# this function consults the cache matrix to see if inverse is already computed
# if so, it fetches from the cache. else, it computes and stores the inverse.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# test - this function exercises makeCacheMatrix() and cacheSolve()
#   e.g. testCacheMatrix(1:4, 2, 2)
#
testCacheMatrix <- function(i, x, y) {
    message("creating cache matrix..")
    m <- makeCacheMatrix(matrix(i, x, y))
    
    message("invoking cacheSolve..")
    inverse0 <- cacheSolve(m)
    str(inverse0)
    
    message("invoking cacheSolve..")
    inverse1 <- cacheSolve(m)
    str(inverse1)
}
