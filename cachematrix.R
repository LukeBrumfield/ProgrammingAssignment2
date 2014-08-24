## makeCacheMatrix
## Coursera R Programming | Programming Assignment 2

## Caches the inverse of a matrix to avoid wasted computation time.
## Uses format provided in makeVector

##Usage z <- makeCacheMatrix(matrix) Assumes invertible matrix is passed
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  ##WARNING if using z$set(matrix), will clear calculated inverse
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
  ##Returns list of functions
  ##Usage z$get(), etc
  ##WARNING calling set() not recommended
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Tests to see if the inverse has already been computed, and returns it.

##Usage cacheSolve(z)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #Gets the inverse(if it has been computed) from the list created by makeCacheMatrix
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }else{
    message("setting cached data for new matrix")
    m <- x$setinverse()
    return(m)
  }
}

