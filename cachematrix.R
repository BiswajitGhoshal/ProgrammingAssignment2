## These functions will test the Lexical Scoping, implemented
## in R. Though m is defined in in makeCacheMatrix, it is assigned
## value through cacheSolve through Lexical Scoping.

## Written by Biswajit Ghoshal.

## makeCacheMatrix function defines the set, get, setInverse and
## getInverse functions, which are to be performed on a 
## Invertible Matrix. It is assumed that the matrix received
## is Invertible.  set and get will be used by the user,
## while setInverse and getInverse will be called from 
## cacheSolve function, in it's Lexical scope context.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function should be called after running makeCacheMatrix
## and after defining and 'set'ting the matrix, for which inverse
## has to be computed. This function will either return the computed
## or cached Inverse of the matrix, defined through the set function, 
## in it's Lexical scope.  The argument x is a list of 4 functions, 
## defined through makeCacheMatrix.  

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
