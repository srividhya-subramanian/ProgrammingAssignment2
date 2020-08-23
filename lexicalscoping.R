
makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL     #initializing inverse as null
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x     #function to obtain matrix 'x'
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv  #function to get inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


cacheSolve <- function(x, ...)       #to obtain cache data
{
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)       # returns the inverse
        }
        mat <- x$get()
        inv <- solve(mat, ...)  #calculates the inverse value
        x$setInverse(inv)
        inv         #returns a matrix which is inverse of 'x'
}
