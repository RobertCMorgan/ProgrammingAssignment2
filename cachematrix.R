## These functions work together to create a special object that stores
## a matrix and caches its inverse. 
 

## This frist function makeCacheMatrix creates a special "vector", which 
## is really a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize matrix as NULL
  i <- NULL
  
  ## Create function to set value of matrix in different environment
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
    
  get <- function() x ## Function to get matrix
  setinv <- function(solve) i <<- solve  ## Function to set inverse of matrix
                                         ## in different environment
  getinv <- function() i ## Function to get inverse of matrix
  
  ## Create a list to contain all the functions 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve function - calculates the inverse of the matrix 
## created in makeCacheMatrix function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the cache via 
## the setinv function.


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  
  i <- x$getinv()  ## Attempt to assign i the value of inverse matrix 
                   ## from cache
  
  ## Return value of inverse matrix if it was successfully assigned
  if(!is.null(i)) { 
    message("getting cached data")
    print("Hello1")
    return(i)
  }
  
  ## Calculate inverse matrix if not found in cache
  data <- x$get() 
  i <- solve(data, ...)
  x$setinv(i)
  print("Hello2")
  i
    
}

