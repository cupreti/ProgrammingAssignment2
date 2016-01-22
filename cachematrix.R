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
  calInverse <- x$getInverse()
  
  if (!is.null(calInverse) &&
      is.matrix(calInverse)) {
    message("From cached data....")
    return(calInverse)
  }
  
  matrixToSolve <- x$get()
  
  calInverse <- solve(matrixToSolve) 

  message("Setting the value of Inverse")
  x$setInverse(calInverse)
}
