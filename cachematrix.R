## makeCacheMatrix creates a special matrix object that can cache its inverse
## and cacheSolve finds the inverse of the special matrix created by makeCacheMatrix. If the special
## matrix created by makeCacheMatrix has not been changed and the inverse has already been solved
## then cacheSolve will get the inverse from the cache.

## makeCacheMatrix takes a matrix as an argument and then stores the information about that matrix
## in a list of functions: set(), get(), setinverse(), and getinverse().

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes the matrix created by the makeCacheMatrix function as its argument and finds the
## the inverse of that matrix. The function then caches the inverse in the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
}


## EXAMPLE RUN:

## > myMatrix <- makeCacheMatrix(matrix(c(3, -2, 2, 1), 2, 2))
## > myMatrix$get()  ## retrieves the matrix you just created
##   [,1] [,2]
##   [1,]    3    2
##   [2,]   -2    1
## > cacheSolve(myMatrix)  ## first run caches the inverse in the function getinverse()
## > cacheSolve(myMatrix)  ## running the cacheSolve function again will retrieve the cached inverse
##   getting cached data
##   [,1]       [,2]
##   [1,] 0.1428571 -0.2857143
##   [2,] 0.2857143  0.4285714
## > myMatrix$set(matrix(c(2, -2, 3, 1), 2, 2)) ## change the matrix values using the set() function
## > myMatrix$get()
##   [,1] [,2]
##   [1,]    2    3
##   [2,]   -2    1
## > cacheSolve(myMatrix) ## caches the new inverse 
## > cacheSolve(myMatrix)
#    getting cached data
##   [,1]   [,2]
##   [1,] 0.125 -0.375
##   [2,] 0.250  0.250
## > myMatrix$getinverse() ## running the getinverse() function will also get the inverse
##       [,1]   [,2]
##   [1,] 0.125 -0.375
##   [2,] 0.250  0.250