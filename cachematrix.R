## The two functions below are used to create a "special" object that
## stores a numeric 'square invertible' matrix and caches its inverse
## thereby reducing the computational cost of re-calculating the inverse
## every time the inverse value is requested.
##
## Note:  It is assumed that the matrix supplied is always invertible


## Creates a special "Matrix", which is really a list containing
## a function to
##
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of the matrix
##   4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Returns the inverse of the given 'square invertible' matrix created
## using the makeCacheMatrix function defined above. To avoid costly
## re-calculations for a given matrix, this function first checks to
## see if the inverse of this matrix has already been calculated. If so,
## it gets the inverse of the matrix from the cache and skips the
## computation.  Otherwise, it calculates the value of inverse of the
## matrix and sets that value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
