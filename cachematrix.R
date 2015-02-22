## Put comments here that give an overall description of what your
## functions do

## Usage:
## mtx <- matrix(1:4, nrow=2, ncol=2)
## cacheMtx <- makeCacheMatrix(mtx)
## cacheSolve(cacheMtx)
## This should return:
##
## [Loading cached data] # only printed if cacheSolve(CacheMtx) has 
##                       # been executed and inverse has been cached
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## creates a cached object containing the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## Return the invers of a matrix either calculated or fetched from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)){
            message("Loading cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
