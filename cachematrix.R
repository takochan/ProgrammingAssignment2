## This file contains funtions that help manage the inversing 
## of matrices. They are written to enable the cacheing of the 
## inverted matrix.

## This function is a wrapper for 4 other functions which help
## cache a matrix and it's inverted partner
makeCacheMatrix <- function(x = matrix()) {
        imatrix <- NULL
        #sets the value for the original matrix
        set <- function(y) {
                x <<- y
                imatrix <<- NULL
        }
        #gets the value of the original matrix
        get <- function() x
        #sets the inverse of the matrix
        setInverse <- function(inverseMatrix) imatrix <<- inverseMatrix
        #gets the inverse of the matrix
        getInverse <- function() imatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function returns the inverse of a matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        #check to see if inverse has already been supplied and if it has return the cached version
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #these lines execute if the cache has not been found (i.e. it is executing for the first time)
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        m
}
