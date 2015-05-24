## Put comments here that give an overall description of what your
## functions do

## This function returns a special vector containing four functions and caches the
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  setmatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getmatrix <- function() x
  setinverse<-function(inv) inverse<<-inv
  getinverse<-function() inverse
  list(setmatrix=setmatrix, getmatrix=getmatrix, 
       setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of a matrix if the inverse doesn't exist
## If the inverse exists, the functions returns the cache value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix<-x$getmatrix()
  inverse<-solve(matrix, ...)
  x$setinverse(inverse)
  inverse
}
