## Put comments here that give an overall description of what your
## functions do


## This function creates a special matrix to store the inverse
## matrix that will be calculated by cacheSolve

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }

get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)

    
}  



## To calculate the inverse of a matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  mtx <- x$get()
  inv <- solve (mtx)
  x$setInverse(inv)
  
  inv
  
}


