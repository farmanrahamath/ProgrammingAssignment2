## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
