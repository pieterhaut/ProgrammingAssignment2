## WEEK 3, PROGRAMMING ASSIGNMENT 2

## The function below creates a special "matrix" which really is a list containing a function to
##  1. Set the value of the matrix
##  2. Get the value of the matrix
##  3. Set the value of the inverse matrix
##  4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function () x 
      setinverse <- function(inversematrix) inv <<- inversematrix
      getinverse <- function() inv
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The function below retrieves the inverse of the matrix created in the funcion 'makeCacheMatrix' (see above) when it is already stored in cache. 
## Otherwise this function computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
      
      inv <- x$getinverse()
      
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
            
      }
      
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv

}
