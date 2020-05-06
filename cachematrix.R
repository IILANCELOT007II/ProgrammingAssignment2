## Put comments here that give an overall description of what your
## functions do

## The function takes a matrix as a parameter and creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){
    x
  }
  setinv <- function(inverse){
    m <<- inv(x)
  }
  getinv <- function(){
    m
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache to reduce computational resources

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
  
}
