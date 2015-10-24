## A pair of functions that return an inverse of a 'matrix' object, by either
## inversing the matrix or retrieving a previously cached inverse


## Function to create a "matrix" object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      setmatrix<- function(y){
          x <<- y
          inv <<- NULL
      }
      getmatrix<- function() x
      setinverse<- function(inverse) inv <<- inverse
      getinverse<- function() inv
      list(setmatrix = setmatrix, 
           getmatrix = getmatrix,
           setinverse = setinverse,
           getinverse = getinverse)
}


## A function that either calculates a matrix inverse or retrieves previously
## cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        data<- x$getmatrix()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
