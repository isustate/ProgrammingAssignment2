## These functions will compute the inversion of a matrix that demonstrates use
## of cacheing in R.
##
## Date Created: 2014-11-21
## Date Updated: 2014-11-21

## makeCacheMatrix  invertes a matrix using the solve function.  Returns a list containing the following.
## set - Initates x and m
## get - Displays matrix
## setinverse -
## getinverse - Displays inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
              x <<- y
              m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function checks to see if the cache contains the inversed matrix.  If so, 
## the inversed matrix is return.  Otherwise, it calls makeCacheMatrix to invert the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m}
