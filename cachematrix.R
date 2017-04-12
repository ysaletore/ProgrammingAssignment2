## Put comments here that give an overall description of what your
## functions do

#' Cache inverse of a matrix
#'
#' Uses a list  to store functions for caching the inverse of a matrix.
#'
#' @param x matrix to find inverse of
#'
#' @return List with corresponding functions:
#'   get: get current matrix
#'   set(y): set current matrix to y
#'   setinverse: set the inverse matrix
#'   getinverse: get the inverse matrix
#'
#' @export
makeCacheMatrix <- function(x = matrix()) {
  # initialize the cache to NULL
  cachedInverse <- NULL
  
  # set function
  set <- function(y) {
    x <<- y
    
    # reset the cache to NULL
    cachedInverse <<- NULL
  }
  
  # get function
  get <- function() {
    x
  }
  
  # setinverse function
  setinverse <- function(newInverse) {
    cachedInverse <<- newInverse
  }
    
  # getinverse function
  getinverse <- function() {
    cachedInverse
  }
  
  # now return the list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' Calculate inverse of a matrix using makeCacheMatrix
#'
#' Uses the list from makeCacheMatrix to cache the inverse
#' of the matrix, and use the cache if the matrix remains unchanged.
#'
#' @param x matrix to find inverse of
#'
#' @return List with corresponding functions:
#'   get: get current matrix
#'   set(y): set current matrix to y
#'   setinverse: set the inverse matrix
#'   getinverse: get the inverse matrix
#'
#' @export
cacheSolve <- function(x, ...) {
  # get cached inverse
  cachedInverse <- x$getinverse()
  
  # if this is not NULL, we can use it
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  
  # otherwise, we must compute the inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
