## Put comments here that give an overall description of what your
## functions do

## create a new object which stores a matrix and allows to set an inverse
## a list of functions returned by this function
## gives acccess both the matrix and its inverse
##
## inverse needs to be set by some other code (not calculated internally)


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  #inverse
  
  set <- function(y) {
    x <<- y
    i <<- NULL #reset cached inverse
  }
  
  get <- function() x
  
  setinverse <- function(inv) i <<- inv
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## takes an object returned by makeCacheMatrix as an input
## returns an inverse of the matrix passed to makeCacheMatrix
## if inverse is not available in the cache it's calculated
## and cached
## simple test: round(cacheSolve(mat) %*% x) returns an identity matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse();
  if(!is.null(i)) {
    
    #message("getting cached inverse")
    
    return (i)
  } else {
    
    #message("setting inverse")
    
    x$setinverse(solve(x$get()))
    x$getinverse()
  }
  
}
