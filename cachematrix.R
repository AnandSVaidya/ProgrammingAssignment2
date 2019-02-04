## Because matrix inversion is costly computation, it is beneficial to cache inverse of matrix
## Below two functions provides ability to create special matrix that can cache its inverse
## and provides ability to use the cached matrix for calculating inverse

## makeCacheMatrix creates a special matrix that can cache its inverse. 
## The function assumes the matrix provided as input is always invertible

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
  
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
  
    get <- function() x
    
    setInverse <- function(inverse) im <<- inverse
    
    getInverse <- function() im
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed, 
## then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getInverse()
    
    if(!is.null(im)) {
      message("Getting cached matrix")
      return(im)
    }
    
    data <- x$get()
    im <- solve(data, ...)
    x$setInverse(im)
    im
}
