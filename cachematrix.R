## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates an object to store a matrix, and also
# its cache value

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse variable
  inverse <- NULL
  
  # define set function
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  # define get function
  get <- function() x
  
  # define setter and getter of inverse variable
  setinverse <- function(inverseInput) inverse <<- inverseInput
  getinverse <- function() inverse
  
  # return the object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function retrieves cache inverted matrix of makeCacheMatrix
# object. If no such cache exist, a new cache will be calculated.
# This function assumes input matrix is invertible

cacheSolve <- function(x, ...) {
  # get cache value
  inverse <- x$getinverse()
  
  # cache exist, return it
  if(!is.null(inverse)){
    message("getting cached matrix inverse")
    return(inverse)
  }
  
  # cache does not exist, calculate it
  matrix <- x$get()
  inverse <- solve(matrix)
  
  # store calculation result as new cache
  x$setinverse(inverse)
  
  # return result
  inverse
}

