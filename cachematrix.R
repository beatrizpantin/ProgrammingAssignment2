## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## when the makeCacheMatrix function is called, it initializes the variable (R object) inv to NULL, 
## and defines the 4 functions set, get, setmean and getmean. 
## The object inv will (later) hold the inverse of the matrix x that makeCacheMatrix was called with. 



makeCacheMatrix <- function(x = matrix()) {
   inv<- NULL
   set<-function(y){
     x<<-y
     inv<<-NULL
   }
   
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache. 
##Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'inv <- x$getinverse()
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
