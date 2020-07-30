## It takes in a matrix and returns a list consisting of 4 functions. 

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
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
