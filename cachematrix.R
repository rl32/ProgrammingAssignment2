##The following two functions is used to cache the inverse matrix of one matrix.
##To get a inverse of matrix is potentially time-consuming computations.
##So we just compute the inverse for a matrix when the inverse is firstly been accessed, 
##then save the result. The result can be used for the subsequent accessing as long as the original 
##matrix has not changed.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list including four functions: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {    
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache
## the parameter x should be the special "matrix" returned by makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        inverse <- x$getinverse()
        if(is.null(inverse)) {              
                data <- x$get()
                inverse <- solve(data, ...)
                x$setinverse(inverse) 
        }
        inverse
}
