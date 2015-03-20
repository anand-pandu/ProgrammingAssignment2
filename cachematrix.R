## The overall program function:
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix:
## The following function, makeCacheMatrix creates a 
## special matrix, which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean


makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    set <- function(y) 
      {
        x <<- y
        i <<- NULL
      }
    get <- function() x
    # Set inverse
    setinverse <- function(inverse) i <<- inverse
    # Get inverse
    getinverse <- function() i
    list(set = set, get = get,
           setinverse = setinverse,
         getinverse = getinverse)
  
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.The execution time greatly improved when the data is retreived
# from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # check if Cached data already available
  
  if(!is.null(m)) 
    {
      message("getting cached data")
      return(m)
    }
  
  data <- x$get()
  
  ## compute inverse by calling solve function
  m <- solve(data)
  x$setinverse(m)
  m
}
