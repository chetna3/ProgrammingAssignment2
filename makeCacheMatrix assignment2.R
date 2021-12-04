makeCacheMatrix <- function(x = matrix()) {
  sinv <- NULL
  set <- function(y) {
    x <<- y
    sinv <<- NULL
  }
  get <- function() (x)
  setinverse <- function(inverse) {sinv <<- inverse}
  getinverse <- function() (sinv)
  list(set = set, get = get,setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  sinv <- x$getinverse()
  if(!is.null(sinv)) {
    message("getting cached data")
    return(sinv)
  }
  mat <- x$get()
  sinv <- solve(data, ...)
  x$setinverse(sinv)
  sinv
}