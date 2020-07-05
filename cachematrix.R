## In this function i set x as a matrix
## Then the solved value is m and is a NULL
## this exercise is about caching the inverse of a matrix so all the "solve" 
## Were changed to "Inverse"
## This "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## In this case, again the "solve" were changed to "Inverse" 
## This function computes the inverse of the special "matrix" returned
## By makeCacheMatrix above. If the inverse has already been calculated 
## (And the matrix has not changed), then cacheSolve should retrieve the inverse
## From the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached results")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
