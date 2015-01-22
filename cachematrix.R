## PROGRAMMING ASSIGNMENT 2: Caching the Inverse of a Matrix ##


## The two functions below allow us to create and access a matrix and its inverse, 
## including, if appropriate, access a cached version of the inverse matrix


## This function returns a list containing a number of other functions which can be used to 
## create a matrix and its inverse and to access both objects
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function() i <<- solve(x)
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function takes the list returned from makeCacheMatrix as an argument. It retruns
## the inverse matrix from the cache if it exists, otherwise it creates the inverse matrix then 
## returns it

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)             
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}
