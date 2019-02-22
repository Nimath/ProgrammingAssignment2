## The input of the first function is a matrix and the output
##  is really a list of funtions as follows:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL ## the value of the inverse of x
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    } ## set the value of the matrix x
    get <- function() x ## get the value of the matrix
    setinverse <- function(inverse) xinv <<- inverse ## set the value of the inverse
    getinverse <- function() xinv ## get the value of the inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinverse()
    if (!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinverse(xinv)
    xinv
    }
