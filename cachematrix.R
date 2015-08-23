## Functions for creating and using inverted matrices which caching ability


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Set the value of the matrix
        set <- function(y) {  
                x <<- y
                m <<- NULL
        }
        
        # Get the value of the matrix
        get <- function() x  
        
        # Set the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve
        
        # Get the value of the inverse of the matrix
        getsolve <- function() m
        
        # Return our list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)  
}



## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) { # 'x' is a 'makeCacheMatrix' object
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()        # Get cache
        if(!is.null(m)) {        # Is inverse cached?
                message("getting cached data")
                return(m)        # if yes, then return cached
        }
        data <- x$get()          # else, get matrix
        m <- solve(data, ...)    # computer inverse of matrix
        x$setsolve(m)            # cache inverse
        m                        # return inverse
}


## Example:
##    > mymatrix <- matrix(c(1,2,3,0,1,4,5,6,0), nrow=3, ncol=3, byrow=TRUE)
##    > mCM <- makeCacheMatrix(mymatrix)
##    > cacheSolve(mCM)
##    ... outputs inverse ...
##         [,1] [,2] [,3]
##    [1,]  -24   18    5
##    [2,]   20  -15   -4
##    [3,]   -5    4    1
##
##    > cacheSolve(mCM)
##    ... outputs inverse from cache ...
##    getting cached data
##         [,1] [,2] [,3]
##    [1,]  -24   18    5
##    [2,]   20  -15   -4
##    [3,]   -5    4    1


