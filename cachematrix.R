## The makeCacheMatrix has the functions get (which takes the input matrix), 
## setinv(which sets the inverse matrix to inv), 
## getinv(which takes the inverse matrix as input if already cached)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve matrix first tries to check if the inverse matrix has already been calculated
## and stored in the cache. If yes, it returns the cached value. Else, it calculated the inverse
## using the solve function (inbuilt R function) and returns the same

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
