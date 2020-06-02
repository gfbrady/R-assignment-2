## Together these functions allow you to "solve" (produce the inverse of) a matrix,
## store that inverse to the cache, and retrieve it.

## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This second function will return the cached inverse if it already exists; otherwise,
## it will generate the inverse and print it to the console.

## Note: input for this second function should be an object created as the output of the
## first function.
## Example: output <- makeCacheMatrix(x) where x is a square matrix.
## Then, run cacheSolve(output) to display the inverse of matrix x.

cacheSolve <- function(x, ...) {
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