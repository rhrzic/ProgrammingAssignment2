## Put comments here that give an overall description of what your
## functions do

## This function can be understood as a wrapper for the original matrix function. The added functionality is that it creates a structure (list)
## around the created matrix that allows it to save its inverse and avoid calculating it again.
## There is a bit of sorcery in the beginning regarding the parameters (arguments) of this function, because I wanted to be able to pass either
## an alreaady existing matrix or instructions to construct a new matrix (eg. makeCacheMatrix(rnorm(100), 10, 10)).

makeCacheMatrix <- function(...) {
  
  ## Get the arguments passed and collect them into a list
  params <- list(...)
  
  ## If there is only one argument passed and it is a matrix, then we will use this matrix to continue.
  if (length(params)==1 & class(params[1]) == "matrix"){x <- ...}
  
  ## In other cases we assume that the user is trying to pass arguments/instructions to create a matrix, so we handle that.
  else {x <- matrix(...)}
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list (set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}


## This function checks the matrix object (the argument) for the inverse. If it exists, it signals that it will return the cached inverse.
## If it doesn't yet exist, it calculates it and stores it into the the matrix object for later retrieval.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
