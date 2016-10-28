# refactored-enigma

makeCacheMatrix <- function(x = matrix()) {
      d <- NULL
      set <- function(y) {
            x <<- y
            d <<- NULL
      }
      
      get <- function() x
      setinverse <- function(solve) d <<- solve
      getinverse <- function() d
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
      d <- x$getinverse()
      if(!is.null(d)) {
            message("getting cached data")
            return(d)
      }
      data <- x$get()
      d <- solve(data, ...)
      x$setinverse(d)
      d
}
