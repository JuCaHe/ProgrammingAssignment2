## Put comments here that give an overall description of what your
## functions do

#**This next couple of functions will create a Matrix and then
#return the inverse of this matrix.**

## Write a short comment describing this function

#**makeCacheMatrix is the function that will create the 
#matrix that will be used later to be computed.**

makeCacheMatrix <- function(x = matrix()) { 
# ** x is set as a MATRIX. Matrices can be inverted**
        m <- NULL
# **m will be used to set the cache, initially m is null
# because nothing is supposed to be cached**
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
# **New function defined: solve. It's going to be used by the next 
# function (cacheSolve) to run the inverse of x.
# The argument is now cache in m.**
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

# **cacheSolve will return the inverse of the matrix created 
# by makeCacheMatrix. **

cacheSolve <- function(x, ...) {        
		m <- x$getinverse()
# **If there's something cached in m, then return it.**
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
# ** If nothing is cached, then run the solve function over
# the matrix and assign it to m.**
        m <- solve(data, ...)
# **data will return the matrix from makeCacheMatrix, and by 
# using the solve function we can get the inverse. **
        x$setinverse(m)
# **return m, with the final inverse value of the 
# original matrix.**
        m
        ## Return a matrix that is the inverse of 'x'
}
