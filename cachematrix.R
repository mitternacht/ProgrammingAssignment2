## Creates a special object that stores an invertible matrix and caches its inverse.
## The special object is really a list containing functions to:
##        
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse
##      get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                m <<- inverse               
        }
        getinverse <- function(){
                m
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created with the function "makeCacheMatrix". 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting cached data.")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
