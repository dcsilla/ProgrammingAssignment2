## Assignment for Coursera's "R Programming" course
## The functions can be used to count and cache the inverse
## of an inversible square matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                              #initially inverse is NULL
  set <- function(y) {                     #if new matrix changes, then
    x <<- y                                #new value is given and
    inv <<- NULL                           #inverse is set as "not computed"
  }
  get <- function() x                      #returns matrix
  setinv <- function(solve) inv <<- solve  #computes inverse
  getinv <- function() inv                 #returns inverse
  list(set = set, get = get,               #returning functions on special matrix
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                 #check if inverse exists
  if(!is.null(inv)) {               #if inverse exists, return cached data and message
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                   #else, request for the values of the matrix
  inv <- solve(data, ...)             #and compute inverse
  x$setinv(inv)                       #cache new inverse
  inv  
}
