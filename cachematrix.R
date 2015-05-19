## These functions allow you to create an object which contains 
##  a matrix and it's inverse, and several functions to set the 
##  matrix and it's inverse, and pull it from memory, rather 
##  than recalculating it.
##  Examples:
##  > x <- makeCacheMatrix() ## Creates the object x, of the type
##                              makeCacheMatrix
##  > x$set(matrix(1:4,2,2)) ## Set the matrix to a 2x2
##  > x$get()                ## Return the stored matrix
##      [,1] [,2]
##  [1,]   1    3
##  [2,]   2    4
##  >cacheSolve(x)           ## Solve for the inverse of x
##      [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  >cacheSolve(x)           ## inverse already solved, returns cache
##  getting cached data
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

## function makeCacheMatrix:
## The purpose of this function is to create a list containing 4
##    functions, while storing a matrix and it's inverse for later
##    use. The 4 functions are:
##    * set - allows you to set the matrix
##    * get - returns the previously stored matrix
##    * setinverse - sets the other stored value, the inverse of 
##        the matrix
##    * getinverse - returns the inverse of the matrix, if it has
##        been set using the cacheSolve function.


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


## function cacheSolve:
## The purpose of this function is to return the inverse of the object x. x 
##   is a list of functions created using the makeCacheMatrix function.
## If the inverse has previously been solved, then it returns the previously
##   solved result, which is stored in the list x created by the function 
##   makeCacheMatrix. Otherwise, it calculates the inverse, and saves it in the
##   list x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
