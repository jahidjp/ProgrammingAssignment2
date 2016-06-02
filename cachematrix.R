## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(i=matrix()) {
      m <- NULL
      set <- function(j) {
            i <<- j
            m <<- NULL
      }
      get <- function() i
      setInverse <- function(solve) m <<- solve
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

cacheSolve <- function(i, ...) {
      m <- i$getInverse()
      if(!is.null(m)) {
            message("Get cached data")
            return(m)
      }
      data <- i$get()
      m <- solve(data, ...)
      i$setInverse(m)
      m
}
