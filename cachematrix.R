
# The makeCacheMatrix function creates a special matrix object
# that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setm <- function(inverse) {m <<- inverse}
  getm <- function() {m}
  list(set = set, get = get, 
       setm = setm,
       getm = getm)
}


# The cacheSolve function calculates the inverse of the matrix.
# However, it first checks to see if the inverse has already been 
# calculated. If so, it gets the inverse from the chache and skips 
# the computation. Otherwise, it calculates it and sets the inverse
# in the cache. 

cacheSolve <- function(x, ...) {
  m <- x$getm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setm(m)
  m
}
