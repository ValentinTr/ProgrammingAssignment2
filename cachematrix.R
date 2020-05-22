## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function create a matrix that can have in cache is inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  
}


## Write a short comment describing this function
##If the matrix have inverse in cache the function return his inverse
##If the matrix doesnt have inverse in cache the function calculate the inverse and set in cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

