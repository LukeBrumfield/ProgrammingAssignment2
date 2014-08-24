## makeCacheMatrix
## Coursera R Programming | Programming Assignment 2

## Caches the inverse of a matrix to avoid wasted computation time.
## Uses format provided in makeVector
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(){
    m <<- solve(x)
  }
      
  getinverse <- function(){
    m
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Tests to see if the inverse has already been computed, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$solve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

