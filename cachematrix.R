## These two functions can be used to calculate the inverse of a square invertible matrix 
## and caches the result in a matrix object.

## Returns a special "matrix" object that can cache its inverse
## The object contains getters and setters for the matrix and inverse values
makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inverse <<- inv
  getInv <- function() inverse
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Returns a matrix that is the inverse of 'x', which is an object created by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  
  inverse <- x$getInv()
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInv(inverse)
  inverse
}
