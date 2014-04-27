## The two functions outlined below - makceCacheMatrix and cacheSolve
## are used to create a wrapper around a numeric matrix. The wrapper 
## provides accessor methods to get / set the values of the matrix 
## and its inverse, so that is possible to cache the inverse.

## This functions provides a list of accessor methods on the input
## matrix. Pass an invertible numeric matrix to this function whose
## inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
  
      xinv <- NULL
      set <- function(y)  {
        
        x <<- y
        xinv <<- NULL
      }
      
      get <- function() x
      
      setinv <- function(minv) xinv <<- minv
      
      getinv <- function() xinv
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function calls makeCacheMatrix to see if the inverted 
## matrix is already cached; if not, it computes the inverse 
## using 'solve' and invokes the setter to store the inverse
## Pass a wrapper list output from makeCacheMatrix as input to
## this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv))  {
          
          message("getting cached data")
          return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
