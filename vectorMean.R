makeVector <- function(x = numeric()) {      # input x will be a vector
  
  m <- NULL    #  m will store our 'mean' and it's reset to NULL every 
  #    time makeVector is called
  
  #  note these next three functions are not run when makeVector is called.
  #   instead, they will be used by cachemean() to get values for x or for
  #   m (mean) and for setting the mean.  These are usually called object 'methods'
  
  get <- function() { x }   # this function returns the value of the original vector
  
  setmean <- function(mean)  # this is called by cachemean() during the first cachemean()
  { m <<- mean }  #  access and it will store the value using superassignment
  
  getmean <- function() { m } # this will return the cached value to cachemean() on
  #  subsequent accesses
  
  list(get = get,          #  This list is returned with the newly created object.       
       setmean = setmean,  #   It lists all the functions ("methods") that are part of
       getmean = getmean)  #   the object.  If a function is not on the list then it cannot
  
}

cachemean <- function(x, ...) {    # the input is an object created by makeVector
  m <- x$getmean()               # accesses the object 'x' and gets the value of the mean
  if(!is.null(m)) {              # if mean was already cached (not NULL) ...
    
    message("getting cached data")  # ... send this message to the console
    return(m)                       # ... and return the mean ... "return" ends 
    #   the function cachemean(), note
  }
  data <- x$get()        # we reach this code only if x$getmean() returned NULL
  m <- mean(data, ...)   # if m was NULL then we have to calculate the mean
  x$setmean(m)           # store the calculated mean value in x (see setmean() in makeVector)
  m                      # return the mean to the code that called this function
}