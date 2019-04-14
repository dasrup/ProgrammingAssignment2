## Put comments here that give an overall description of what your
## functions do

## The function (makeCacheMatrix) creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inverseMatrix <- NULL
  set <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverseMatrix <<- solve
  getInverse <- function() inverseMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function (cacheSolve) computes the inverse of the matrix returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setInverse(inverseMatrix)
  inverseMatrix
}

