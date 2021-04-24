## These functions implement caching of a matrix and its inverse
## cacheObj <- makeCacheMatrix(matrix) creates a cache matrix object.
## cacheObj$get() returns the stored matrix in cache.
## cacheObj$set(matrix) updates the matrix stored in cache.
## cacheSolve(cacheObj) fetches the inverse from cache or solves if not present.

## creates a cache matrix object

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## fetches the inverse from cache or solves if not present and stores it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
