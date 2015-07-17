## These functions are used for calculating and storing matrix inverses.
## They were written by Dylan Zwick (dylanzwick@gmail.com).

## This function creates a matrix object from a matrix. The object stores the matrix,
## along with possibly the matrix inverse. The matrix is set and retrieved by the
## functions setMatrix and getMatrix, respectively. The inverse is set and retrieved
## by the function setInverse and getInverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(theInverse) inv <<- theInverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes a matrix object created by the makeCacheMatrix function. It returns
## the cached inverse, if this cached inverse is not null. If the cached inverse is null,
## then the inverse is calculated, stored in the matrix object, and returned.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("Cached Inverse")
      return(inv)
    }
    dataMatrix <- x$getMatrix()
    inv <- solve(dataMatrix, ...)
    x$setInverse(inv)
    inv
}