## The following functions compute matrix's inverse based on cache method
## in order to prevent re-computing.

## "makeCacheMatrix" creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_mat <<- inverse
  getinverse <- function() inv_mat
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)

}


## "cacheSolve" computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then
## "cacheSolve" will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinverse()
  if (!is.null(inv_mat)) {
    message("Getting cache data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setinverse(inv_mat)
  inv_mat
}
