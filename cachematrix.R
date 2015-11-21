## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 

## The makeCacheMatrix function creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) cache <<- inverse
        getinv <- function() cache
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        cache <- x$getinv()
        if(!is.null(cache)) {
                message("getting cached data.")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data)
        x$setinv(cache)
        cache
}

## Test
## > a <- matrix(1:4,2,2)
## > a
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > b <- makeCacheMatrix(a)
## > cacheSolve(b)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(b)
## getting cached data.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
