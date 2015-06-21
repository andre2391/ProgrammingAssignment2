## These functions can be used to compute the inverse
## of a 2x2 matrix. The value of the most recent 
##computed matrix is stored and retreived if the 
## function is called again for the same matrix

##It is assumed that the matrix is invertible

## The makeCacheMatrix 

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
  setinv <- function(inv) inverse <<- inv
  
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  if (x == makeCacheMatrix$get && !makeCacheMatrix$getinv == matrix(nrow = 2, ncol = 2)){
    return(makeCacheMatrix$getinv)  
  }
  else {
    makeCacheMatrix$set(x)
    makeCacheMatrix$setinv(solve(x))
    print(makeCachematrix$getinv())
  }
  
        ## Return a matrix that is the inverse of 'x'
}
