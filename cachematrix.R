## These functions can be used to compute and cache
## the inverse of a matrix. The value of the most recent 
## computed inverse is stored and retreived if the 
## function is called again for the same matrix

##It is assumed that the matrix is invertible

## The makeCacheMatrix must be run first. The only input
## is the matrix to be inverted

makeCacheMatrix <- function(x = matrix()) {
  inverse <- matrix(nrow = 2, ncol = 2) ## Initialize inverse matrix locally
  
  ## Store the matrix being inverted and 
  ## initialize inverse matrix in enclosing 
  ## environment
  set <- function(y) {
    x <<- y
    inverse <<- matrix(nrow = 2, ncol = 2)
  }
  get <- function() x ## Retrieve the matrx that was inverted
  setinv <- function(inv) inverse <<- inv ## Set the cached inverse
  
  getinv <- function() inverse ## Retrieve the cached inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Run this function to compute the matrix inverse 
## or retrieve if previously computed

cacheSolve <- function(x, ...) {
  ## Check if the inverse has been previously computed
  if (!identical(x$getinv(), matrix(nrow = 2, ncol = 2))){
    message("Retreiving from cache")
    x$getinv() 
  }
  else {
    message("Computing inverse")
    x$setinv(solve(x$get(), ...))
    x$getinv()
  }
  
}
