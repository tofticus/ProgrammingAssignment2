## These functions cache the inversion of a matrix, assuming it is invertible,
##and use that cached matrix, if available for use, to solve the inversion.

## The makeCacheMatrix function caches the inversion of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function solves for the inversion of a matrix, and uses the 
##cached value if it is available.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}