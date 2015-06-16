## Functions to calculate the inverse of a given square matrix, store the result in 
## cache and reuse this stored value if needed again.
## 
## Usage: First assign makeCacheMatrix() on the input matrix to an object and then 
## apply cacheSolve() to that object.
## Example: 
##  z <- makeCacheMatrix(matrix(c(1:4),2,2))
##  cacheSolve(z)
## 
## Input: An invertible matrix.
## Output: The inverse of the input (plus messages about cache usage).

## makeCacheMatrix() function creates a list with 4 functions for:
## - storing/modifying the input data and initializing the result - set()
## - getting the input data - get()
## - storing/modifying the result - setinverse()
## - getting the result - getinverse()

makeCacheMatrix <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) i <<- mean
    getinverse <- function() i
    ## Returns the list composed of 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve() function looks for the inverse matrix value in cache. 
## If found, returns it with a message informing about cache retrieval.
## If not, calculates the inverse, stores it in the cache and returns it.

cacheSolve <- function(x, ...) {
    ## Tries to get the inverse from cache
    i <- x$getinverse()
    ## If found the result in cache, returns it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## If not found, gets the input data, calculates its inverse, stores it in cache 
    ## and returns it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

