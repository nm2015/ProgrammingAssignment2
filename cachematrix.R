## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly. The following functions caches the inverse of a matrix


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
              inv <- NULL
              set <- function(y) {
                x <<- y
                inv <<- NULL
              }
              get <- function() x
              setinversedmatrix <- function(inversedmatrix) inv <<- inversedmatrix
              getinversedmatrix <- function() inv
              list(set = set, get = get,
                   setinversedmatrix = setinversedmatrix,
                   getinversedmatrix = getinversedmatrix)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed, then the cacheSolve should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getinversedmatrix()
            if(!is.null(inv)) {
              message("getting cached data - Inversed matrix")
              return(inv)
            }
            data <- x$get()
        ## Create inverse of the matrix if it does not exist in cache    
            inv <- solve(data, ...)
            x$setinversedmatrix(inv)
            inv
}
