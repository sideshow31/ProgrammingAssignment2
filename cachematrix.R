##  The functions below (makeCacheMatrix and cacheSolve) leverage and take advantage of lexical 
##  scoping behavior in R in order to streamline complex and/or compute intensive calculations. This 
##  accomplished by creating objects to store/cache specific values (in this case the inverse of the
##  matrix) in the global environment.


## The makeCacheMatrix function below creates a special matrix object to store its inverse in the 
## global environment, essentially caching it for use by subdequent function calls.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) m <<- solve 
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## The function below checks to see if the inverse has already been calculated (and if the matrix
## has not changed). If these conditions are met, then cachesolve retrieves and returns the 
## inverse from cache. If not, this function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
