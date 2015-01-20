## `makeCacheMatrix` creates a special "matrix" object that can cache its inverse.
## `cacheSolve` computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above.
## However, if the inverse has already been calculated then `cacheSolve` skips
## the computation and simply retrieves the inverse from the cache.


## The first function, `makeCacheMatrix` creates a special "matrix", which is
## actually a list containing 4 functions:
## 1. `set` stores the value of the matrix in the cache
## 2.  `get` retrieves the value of the matrix in the cache
## 3.  `setinv` stores the value of the inverse in the cache
## 4.  `getinv` retrives the value of the inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) I <<- inverse
      getinv <- function() I
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function `cacheSolve` calculates the mean of the special "matrix"
## created with `makeCacheMatrix`.
## However, it first checks to see if the inverse has already be computed.
## If so, it `get`s the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix input and sets 
## the value of the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      I <- x$getinv()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      data <- x$get()
      I <- solve(data,...)
      x$setinv(I)
      I
}
