## These functions cache the inverse of a matrix. 

## makeCacheMatrix creates a vector of functions (set,get,setinv,getinv)
## enabling us to cashe and get the values of a vector and its inverse
makeCacheMatrix <- function(x=matrix()) {
     inv <- NULL
     set <- function(y) {
          print(y)
          x <<- y
          print(x)
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## CashSolve get us the inverse of a matrix.
## Returns the cashed inverse if exists. Otherwise calculate the inverse and cashe it.
cacheSolve <- function(x, ...) {
     inv<-x$getinv()
     if(!is.null(inv)) {
          message("getting cached inverse")
     }
     else {
          data <- x$get()
          inv <- solve(data, ...)
          x$setinv(inv)
     }
     inv
}
