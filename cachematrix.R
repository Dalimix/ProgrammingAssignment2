## The following functions supply tools to cache te results of the inverse of a matrix.

## The makeCache Matrix creates a matrix with a special set of functions to access the values if
## the matrix itself and of its inverse.
## 
## To use the function one must use the following structure:
## result <- makeCacheMatrix( data )
## where the result will hold the special type of matrix and the data must be a inverteble matrix.
##
## The functions result$get() and  result$set( data ) can then be used to respectively get the values
## of the original matrix and to change it for a new data.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The cacheSolve function can be used to obtain the inverse of a matrix created by the makeCacheMatrix
## function. It must be used as following:
##
## cacheSolve( matrix )
##
## Where matrix must be an inverteble matrix created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
