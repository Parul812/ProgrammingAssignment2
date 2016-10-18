## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse){
    i <<- inverse
  }
  getinverse <- function(){
    i
  }
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function can inverse the matrix passed by makeCacheMatrix 
## and if the inverse already exits it returns the cache value

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
          message("Getting cached data!")
          return(i)
        }
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        i
}
