## UXDEAB: Coursera Programming 2 Assignment
## The functions below calculates and caches the inverse of a matrix

## This function makeCacheMatrix creates a special vector which 
## sets and gets the value of the matrix and sets and gets its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  
  ##set the value of the vector
  set <- function(y) {
      x <<- y ##caches vector
      m <<- NULL ##caches m
  }
  get <- function() x ##returns value of the vector
  setinverse <- function(solve) m <<- solve ##sets the inverse of the matrix 
  getinverse <- function() m ## returns the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function cacheSolve calculates the inverse 
## of the special matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return (m)
    } 
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}