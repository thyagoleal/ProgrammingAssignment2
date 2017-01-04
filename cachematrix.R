## Programming Assignment 2 - Caching the inverse of a matrix.
### Since caching a matrix requires a lot of computational power, it is more benefitial to cache
### the inverse of a matrix. 

#### Function makeCacheMatrix = creates a special matrix, which is ablet to:
# 1 -> set the value of the matrix;
# 2 -> get the value of the matrix;
# 3 -> set the value of the inversed matrix;
# 4 -> get the value created in latter step.

## 

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function (y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) a <<- inverse
        getinverse <- function()a
        list( set = get,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## The following function calculates the inverse of the special matrix created before. 
# However, it first checks to see if the inverse has already been 
#calculated. If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in 
#the cache via the setinverse function.


cacheSolve <- function(x, ...){
        a <- x$getinverse
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        data <- x$get()
        a <- solve(data, ...)
        x$setinverse(a)
        a
}
