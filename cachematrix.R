## Put comments here that give an overall description of what your
## functions do
# The functions calculate the value of the inverse matrix of a 
# squared invertible matrix and stores it for future references


## Write a short comment describing this function
# makeCacheMatrix creates a list with 4 different function given 
# a square invertible matrix:
# - set: set the value of the matrix;
# - get: get the value of the stored matrix;
# - setinverse: set the value of the inverse matrix;
# - getinverse: get the value of the stored inverse matrix, if it's 
# cached.

makeCacheMatrix <- function(x = matrix()) {
      inv <<-NULL
      set <- function(y) {
            x <<- y
            inv <<-NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, 
           getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve checks if the inverse matrix of a given matrix is 
# already cached in the function list. In a positive case,
# it simply returns the value of the inverse matrix. In a 
# negative case, it compute the inverse matrix with 'solve'
# function, stores the value in the function list and returns
# the value.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <-x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}
