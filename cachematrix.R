## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#This function creates a special "matrix" object that can cache its inverse.       
        inv = NULL
        set = function(y) {
                
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setverse = function(inverse) inv <<- inverse 
        getverse = function() inv
        list(set=set, get=get, setverse=setverse, getverse=getberse)
}
cacheSolve <- function(x, ...) {
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
        inv <- x$getverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setverse(inv)
        return(inv)
}
