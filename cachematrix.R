## The first function initializes a matrix object which can create a cache for it's inverse. 
## The second function returns the inverse by either retrieving the cached inverse, if there is one
## or calculating the inverse if there is no cached inverse stored.

## This function creates a special "matrix" object that can cache its inverse. (from assignment discription)

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

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache. (from assignment discription)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) { # Check to see if there is a cached value already stored
    message("retrieving cached data")
    return(i) # function ends here if if statement is true
  }
  myMatrix <- x$get() # if the if statement is false, function continues to calculate matrix
  i <- solve(myMatrix, ...)
  x$setInverse(i)
  i
}
