## "makeCacheMatrix" creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## initiate an empty matrix to hold output from solve function.
        ma <- matrix(NULL)
        ## define 4 founctions and send objects into global enviroment with "<<-"
        set <- function(y) {
                x <<- y
                ma <<- NULL
        }
        get <- function() x
        setMa <- function(solve) ma <<- solve
        getMa <- function() ma
        ## To put the above 4 functions availale as a list object
        list(set = set, 
             get = get, 
             setMa = setMa,
             getMa = getMa)
}


## "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ma <- x$getMa()
        ## to find out if the same object available in the cache, display a message
        ## saying that and returned the cached value.
        if(!is.null(ma)) {
                message("getting cached data")
                return(ma)
        }
        # otherwise, just do it from scratch.
        data <- x$get()
        ma <- matrix(data, ...)
        x$setMatrix(ma)
        ## Return a matrix that is the inverse of 'x'
        ma
}

