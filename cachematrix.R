## The function 'makeCacheMatrix' is a list of 4 functions that either write values to cache or read them from cache.
## The function 'cacheSolve' is applied to the value assigned with running 'makeCacheMatrix' on a matrix x. (The matrix x is assumed to be square invertible)
## 'cacheSolve' retuns the inverse of matrix x either form cache (if previously calculated) or calculates the inverse, saves it into cache and returns the inverted matrix.


## makeCacheMatrix is a function containing a list of 4 functions:
## First function called 'set' takes the input matrix x and stores it in the cache
## Second function called 'get' returns the matrix from the cache
## Third function called 'setinverse' saves the inverse in the cache
## Forth function called 'getinverse' returns the inverse from the cache
## List is needed to address the 4 functions individually within the main function 'makeCacheMatrix' 

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) I <<- solve
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

        

## The 'cacheSolve' function tests first if the inverse was already calculated previously. 
## If so returns the inverse from the cache 
## Otherwise the inverse is calculated using the solve() function. The result is stored in the cache and returned to console

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}


## This are some test changes for GitHub
