## This pair of functions cache inverse of a given matrix,
## which is a time-consuming computation.

## This function returns a list containing functions to 
##  - Set the value of the matrix;
##  - Get the value of the matrix;
##  - Set the value of the inversion;
##  - Get the value of the inversion.

makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setinversion <- function(INVERSION) INV <<- INVERSION
  getinversion <- function() INV
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}

## The following function calculates the inversion of the special "matrix"...
## ... created with the function above.

cacheSolve <- function(x, ...) {
        ## If the inversion has already been calculated, then it's returned.
  INV <- x$getinversion()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  
## The inversion of the matrix is calculated.
  
  data <- x$get()
  INV <- solve(data, ...)
  x$setinversion(INV)
  INV
}
