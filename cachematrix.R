## The created functions "makeCacheMatrix" and "cacheSolve" should
## be able to create a special matrix object a cache its inverse

## The "makeCacheMatrix" function should create a special 
## matrix object then cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The "cacheSolve" function calculates the inverse of the object
## created by the previous function
## If the inverse has already been computed then it should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting inversed matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}



