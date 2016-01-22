## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  
  
  set <- function(userValue = matrix()) {
    x <<- userValue
    cachedInv <<- NULL
  }
  get <- function() {
    return(x)
  }
  setInverse <- function(invVal) {
    cachedInv <<- invVal
    return(cachedInv)
  }
  
  getInverse  <- function()
    cachedInv
  list(
    set = set, get = get, setInverse = setInverse, getInverse = getInverse
  )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  ## Checking whether the inversed matrix is in catch. If there is catched inverse matrix then this will be displayed
  if (!is.null(inv) &&
      is.matrix(inv)) {
    message("Displaying Cached Inverse Matrix")
    return(inv)
  }
  
  inValue <- x$get()
  ## get the inverse of matrix using solve function
  inv <- solve(inValue)
  
  message("Setting the value of Inverse Matrix")
  x$setInverse(inv)
}
