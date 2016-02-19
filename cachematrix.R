# The functions below provide a mechanism for matrix inversion calculation enhancement.
# As matrix inversion is an expensive operation, in case of the already inverted matrices the results are cached.
# Example usage:
## 
## > mx <- matrix(c(4,6,-2,5,9,-2,-4,9,3), 3, 3)
## > mx
## [,1] [,2] [,3]
## [1,]    4    5   -4
## [2,]    6    9    9
## [3,]   -2   -2    3
## > cmx <- makeCacheMatrix(mx)
## > cacheSolve(cmx)
## [,1]        [,2]   [,3]
## [1,] -1.875  0.29166667 -3.375
## [2,]  1.500 -0.16666667  2.500
## [3,] -0.250  0.08333333 -0.250
## > cacheSolve(cmx)
## getting cached data
## [,1]        [,2]   [,3]
## [1,] -1.875  0.29166667 -3.375
## [2,]  1.500 -0.16666667  2.500
## [3,] -0.250  0.08333333 -0.250



# Creating functions for caching the results of matrix inversion.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(y) {
    i <<- y
    m <<- NULL
  }

  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Returns the inverse of a matrix.
# If the inverted matrix has already been calculated, then it returns from the cache.
# If the matrix has not yet been inverted, then it caches the result.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
