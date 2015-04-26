## These functions help to avoid computing inverse matrices for the same input matrix 
## several times during the task, we can just use the cashes inversed matrix every time

## This function cashes the inverse matrix of the input matrix and allows to change it or
## retrieve it whenever we need

makeCacheMatrix <- function(storedMatrix = matrix()) {
  storedInverse <- NULL
  setMatrix <- function(newMatrix) {
    storedMatrix <<- newMatrix
    storedInverse <<- NULL
  }
  getMatrix <- function() storedMatrix
  setInverse <- function(solution) storedInverse <<- solution
  getInverse <- function() storedInverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse=setInverse,
       getInverse = getInverse)

}


## The following function calculates the inverse of the input matrix
## However, it first checks to see if the inversed matrix has already been calculated. 
## If so, it gets the storedInverse from cache and skips the computation
## Else it calculates the inverse of input matrix and sets the inversed matrix
## in the cache via the `setInverse` function.

cacheSolve <- function(x, ...) {
  storedInverse <- x$getInverse()
  if(!is.null(storedInverse)) {
    message("getting cached data")
    return(storedInverse)
  }
  data <- x$getMatrix()
  storedInverse <- solve(data, ...)
  x$setInverse(storedInverse)
  storedInverse
}
## Return a matrix that is the inverse of 'x'
