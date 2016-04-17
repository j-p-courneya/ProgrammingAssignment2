## "makeCacheMatrix" is a function to create a list of 4 objects (functions) which are 
## 1. set: to set the value of the matrix
## 2. get: to get the value of the input matrix 
## 3. setinv: to set the value of the returned matrix (the inverse of input matrix) 
## 4. getinv: to get the value of the returned martix (the inverse of input matrix) 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv 
  getinv <- function() m
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  ##return a matrix that is the inverse of 'x'
}
