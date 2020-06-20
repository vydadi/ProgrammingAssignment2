## To cache the inverse of a matrix using 2 functions 'makeCacheMatrix' and 'cacheSolve'.
## Assumption is the matrix is invertible squared. 

## This function creates a special "matrix" object that can cache its inverse. As defined in 
## assumption above, the input is an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() {
                x
        }
        setinverse <- function(inverse_1) {
                inverse <<- inverse_1
        }
        getinverse <- function() {
                inverse
        }
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached matrix")
                return(inverse)
        }
        
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setinverse(inverse)
        inverse
        
}
