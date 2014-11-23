## Calculating the inverse of a matrix
## Storing result to avoid continuous recalculation
## usage steps for matrix 'x': 
##   1. y <- makeCacheMatrix(x)   - creates cache
##   2. cacheSolve(y)             - returns inverse

## Creating an extended matrix that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ## getting inverse
  i <- x$getInverse()
  
  ## if inverse has been calculated, return cached data
  if(!is.null(i)) {
    
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  ## else calculating inverse 
  i <- solve(data, ...)
  
  ## ...and setting the cache
  x$setInverse(i)
  i
}
