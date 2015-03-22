## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  temp <- NULL
  
  set <- function(y){
    matrix <<- y
    temp <<- NULL
  }
  
  get <- function() matrix
  
  setInverse <- function(inverse) temp <<- inverse
  
  getInverse <- function() temp
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  temp <- x$getInverse()
  
  if(!is.null(temp)) {
    message("getting cached data")
    return(temp)
  }
  
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m 
}
