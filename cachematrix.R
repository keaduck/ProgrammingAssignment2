## This code is for deriving and caching the inverse of a matrix, then returning the value of the cache.

## The first function, makeCacheMatrix(), accepts a vector (defined in the function's environment as x).
## It defines a cache for the inverse of the vector, x,  as m.
## It sets the value of the cache, m, to NULL every time the function is run.
## Then it defines four functions for inverting x and caching the inverse in m.
## Finally, it creates a list that describes the four functions. This is crucial, because 
## "functions that return objects of type 'list' also allow access to any other objects
## defined in the environment of the function," per Leonard Greski.
## NOTE WELL: the user must assign the output of this function to an object for cacheSolve to accept!
## Example: output <- makeCacheMatrix(x), where "x" is an existing matrix.
## Then: cacheSolve(output).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The second function, cacheSolve(), accepts the object created by running makeCacheMatrix().
## It checks to see if the inverse matrix cache has a non-NULL value. 
## If so, it returns that value. If not, it derives the value, returns it, and populates the cache.
## NOTE WELL: if you run makeCacheMatrix again, it clears the cache. But if you just run cacheSolve again, it doesn't.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}