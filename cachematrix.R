##This file includes two functions: makeCacheMatrix, cacheSolve

#declare and define the function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {  # create a new environment
  m <- NULL             # create a local variable m
  set <- function(y) {
    x <<- y            # use the "superAssignment" operator to assign y to x
    m <<- NULL         # assign value to variable m declared outside this function
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve   # compute the inverse matrix 
  getinverse <- function() m                  # get the inverse matrix
  list(set = set, get = get,                  # create a list of four functions
       setinverse = setinverse,
       getinverse = getinverse)
}

#declare and define the function cacheSolve

# create a function to compute inverse matrix or get from cache

cacheSolve <- function(x, ...) { 
  m <- x$getinverse()              # get value of m from getinverse function
  if(!is.null(m)) {                # if m is not null, print message and
    message("getting cached data")
    return(m)                      # return the m value from cache
  }
  data <- x$get()                  # if m is null, compute the inverse matrix
  m <- solve(data, ...)            # compute inverse matrix
  x$setinverse(m)                  
  m                                # return value of m
}