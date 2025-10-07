## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #inv will hold the cached inverse of x. Set to NULL to indicate no inverse stored yet
  inv <- NULL
  
  #Replacing the matrix with a new matrix y
  set <- function(y){
    x <<- y #<<- assigns new matrix y to the parent environment (function i.e., makeCacheMatrix). This updates the matrix stored in the outer function
    inv <<- NULL #clears cached inverse
  }
  
  get <- function() {x} #returns the matrix currently stored in x
  setInverse <- function(inverse) {inv <<- inverse} #saves computed inverse into the parent environment
  getInverse <- function() {inv} #retrieves the cached value
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached value")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}

m <- matrix(1:4, nrow=2, ncol=2)
cm <- makeCacheMatrix(m)
cacheSolve(cm) #Originally compute matrix inverse
cacheSolve(cm) #Return cached value
