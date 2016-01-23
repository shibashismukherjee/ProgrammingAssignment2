## These functions allow for caching the inverse of a matrix.  
## Inverse of a matrix is a costly computation and this allows the inverse
## to be presented from a cached value if it has already been computed once 
## and the matrix has not changed.

## This function creates a special matrix object that caches its inverse.
## This function takes a matrix as input and returns a special "matrix"  
## which is list of functions that can  
## set the value of the matrix, get the value of the matrix and
## set the inverse of the matrix and get the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invrs) inv <<- invrs 
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the special "matrix" created by
## the above makeCacheMatrix function. It first checks to see if the inverse
## has already been calculated. If so, it returns the cached inverse matrix
## otherwise it computes the inverse and caches it and returns the computed
## inverse. It also prints a message when it is returning a cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}