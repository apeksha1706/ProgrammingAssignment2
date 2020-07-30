## It takes in a matrix and returns a list consisting of 4 functions. 
## set: sets the cached matrix to the new one and sets the inverse to null so that it can be recalculated
## get: gets the cached data
## setinv: sets the cached inverse to the new calculated one in case it is null or the matrix has been changed
## getinv: gets the cached inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## It takes in a list of type that is returned from the function makeCacheMatrix. gets the inverse from the cached data and 
## checks if it is null. If it is, it calculates the inverse using the solve function and sets the inverse in the cached data
## to the new calculated value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
