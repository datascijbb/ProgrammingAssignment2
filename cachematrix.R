## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. The functions below are designed to calculate and cache the
## inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache ts inverse.
## This special "matrix" object is really a list containing a function to
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialise both the inverse, inv, and 
    # matrix data, mat, to be NULL
    # and an empty matrix, respectively.
    inv <- NULL
    mat <- matrix()
    # define the set function
    set <- function(y) {
        # check that y is a valid square matrix
        if(!is.matrix(y)) stop("not a matrix")
        if(nrow(y) != ncol(y)) stop("not a square matrix")
        mat <<- y
        inv <<- NULL
    }
    # initialise our matrix data, mat,  with the input 
    # matrix, x, provided as an argument to this function,
    # using the set function we have just defined
    set(x)
    # define the remaining functions
    get <- function() mat
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" object 
## returned by makeCacheMatrix.
## It first checks to see if the inverse has already been calculated, and 
## if so, (and the matrix has not changed), retrieves the inverse from the 
## cache, skipping the computation. Otherwise, it calculates the inverse of the 
## matrix (using the R 'solve' function) and sets the value of the inverse in
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...)
    x$setinverse(inv)
    inv
}

