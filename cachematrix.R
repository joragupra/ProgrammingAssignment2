## Creates a special matrix suitable for cached inverse calculations.
## These matrices allow getting and setting the matrix itself plus its inverse.
## The returned list contains the functions to do this, which are:
## - get / set : for getting / setting the matrix
## - getinverse / setinverse : for getting / setting the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of a matrix (as returned by the 'solve' function).
## Results are cached to improve performance.
## It requires a special cacheable matrix as input as it is returned by the
## 'makeCacheMatrix' function
cacheSolve <- function(x, ...) {
        
        ## First we look for the inverse in the cache
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        
        ## Compute the inverse and store it in the cache if not found
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}

## Here is how you can use these functions to get the inverse of a matrix and
## cache the results to get better performance:
## 1. Initialize the cacheable matrix
## a <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
## 2. Asking for the matrix inverse will return:
## cacheSolve(a)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## 3. Asking again for the inverse will return the same (notice the message):
## cacheSolve(a)
## getting cached inverse matrix
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##
## And another example with more verbose cacheable matrix initialization:
## 1. Initialize the cacheable matrix
## b <- makeCacheMatrix()
## b$set(rbind(c(1,0), c(0,1)))
## 2. Asking for the matrix inverse will return:
## cacheSolve(b)
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## 3. Asking again for the inverse will return the same (notice the message):
## cacheSolve(b)
## getting cached inverse matrix
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1