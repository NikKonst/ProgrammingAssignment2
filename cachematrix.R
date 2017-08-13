#' NikKonst solution
#' Functions for getting matrix inverse with caching

#' Function creates a special "matrix" object that can cache its inverse
#'
#'@param x Matrix
#'
#'@return List with four functions for set and get matrix and set and 
#'get of inverse of source matrix
#'

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) inv <<- inverse
        getsolve <- function() inv
        
        list(set = set, get = get, setsolve = setsolve, 
               getsolve = getsolve)
}


#'This function computes the inverse of the special 
#'"matrix" returned by makeCacheMatrix above. 
#'If the inverse has already been calculated 
#'(and the matrix has not changed), then the cachesolve 
#'should retrieve the inverse from the cache.
#'
#'@param x A special object which created by makeCacheMatrix function
#'
#'@return An inverse matrix

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
