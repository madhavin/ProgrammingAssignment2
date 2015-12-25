## This program has two functions: makeCacheMatrix and cacheSolve
## you could test this program by sourcing this file
## then create a matrix using a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
## invoke cacheSolve(a) to find out the inverse of the matrix a
## if cacheSolve(a) is invoked again, a message "Getting Cached Inverse" should show up
## and the previous inverse has to be printed again
## now create a different matrix and set it to a.
## invoke cacheSolve(a) to get a different value.


## makeCacheMatrix has four internal functions: set, get, setInverse, getInverse

## set creates a matrix with the given input, initiates cached inverse to NULL 
## get retrieves the already set matrix
## setInverse calculates the inverse of the input matrix
## getInverse retrieves the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  cache_inverse <- NULL
  set <- function(new_matrix) {
    x <<- new_matrix
    cache_inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(new_matrix) {
    cache_inverse <<- new_matrix
    return(cache_inverse)
  }     
  getInverse <- function() {
    cache_inverse
  }     
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve function tries to retrieve an old cached inverse
## if it doesn't exist yet, a new inverse is calculated and is cached as cached inverse
## if it already exists, a message is shown that on old value is being used and the
## cached inverse is printed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache_inverse <- x$getInverse()
  if (!is.null(cache_inverse)) {
    message("Getting Cached Inverse")
    return(cache_inverse)
  }
  now_matrix <- x$get()
  cache_inverse <- solve(now_matrix, ...)
  x$setInverse(cache_inverse)
  cache_inverse
}
