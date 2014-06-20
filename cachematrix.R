## functions to create and get matrix
## and compute the inverse, caching the result

## makeCacheMatrix - 4 sub-functions
## 2 to create and get the matrix
## 2 to set and get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
 ## set up matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
## get the matrix
      get <- function() x
## set up the inverse calculations
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Compute the inverse of the matrix, if not already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
## if inverse  already calculated, get it
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
# compute inverse
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
