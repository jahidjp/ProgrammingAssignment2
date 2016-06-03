## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list to set/get the value of the matrix and
# set/get the value of inverse of the matrix.


makeCacheMatrix <- function(i = matrii()) {
    inv <- NULL
	
    set <- function(j) {
        i <<- j
        inv <<- NULL
    }
	
    get <- function() i
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
	
}

#assumes that the matrix is always invertible
cacheSolve <- function(i, ...) {
    inv <- i$getInverse()
  
    if(!is.null(inv)) {
        message("Get data.")
        return(inv)
    }
	
    data <- i$get()
    inv <- solve(data)
    i$setInverse(inv)
    inv
}
