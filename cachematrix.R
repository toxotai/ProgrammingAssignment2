## Functions implement the process of caching  
## the inverse of a matrix

## makeCacheMatrix function creates an object, 
## that can store the matrix and its inverse
## Input:
## x - a matrix (default value is an empty matrix)

makeCacheMatrix <- function(x = matrix())
{
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) m <<- mean
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## cacheSolve function calculate the inverse of a matrix
## (if it was not calculated already) or returns an cached
## inverse matrix otherwise
## Input:
## x - a square invertible matrix
## ... - other parameters for the function solve()

cacheSolve <- function(x, ...)
{
        m <- x$getinverse()
        if(!is.null(m))
        {
                message("getting cached data")
        }
        else
        {
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)       
        }
        m
}
