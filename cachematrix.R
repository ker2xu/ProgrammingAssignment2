## cacheMatrix.R - cache the inverse of a matrix which saves time 
## when calculate the same matrix'inverse more than one time
## usage: my_matrix <- makeCacheMatrix(origin_matrix)
##        my_matrix_inverse <- cacheSolve(my_matrix[, solve_args])


## Take an matrix object as the only argument and return a list 
## which contains four functions, namely, set, get, setInverse and 
## getInverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Take the return result of makeCacheMatrix
## as the first argument, and other arguments are
## passed to solve function in R, return the 
## inverse of the matrix in x

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}