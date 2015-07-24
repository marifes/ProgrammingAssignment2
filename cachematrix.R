## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedlyo

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
library (MASS)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## to use the "ginv" function we need the "MASS" package. 
        ## It has been included at the beginning of the file.
        
        setmatrix <- function(ginv) m <<- ginv
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrix()
        
        ## If it has been previously run we collect from matrix
        
        if(!is.null(m)) {
                message("getting cached data for matrix")
                return(m)
        }
        data <- x$get()
        m <- ginv(data, ...)
        x$setmatrix(m)
        m
}
