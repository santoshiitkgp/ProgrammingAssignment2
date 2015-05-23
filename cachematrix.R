## the functions will store data (cache) of inverse of a matrix (calculated before )
## and returns the inverse of x  if calculated last time
## lets assume inverse of matrix x is i
 

## makeCacheMatrix will allow to get and set the value of matrix x using x$get() 
##and x$set(y)
##it will also allow to get or set  x`s inverse (get if available) using x$getInverse() and x$setInverse(i)

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will look for inverse of matrix x 
## if calculated before it will return it
## else it will get x from makeCacheMatrix function and calculate its inverse by "solve()"
## then it will set the value of new inverse

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
