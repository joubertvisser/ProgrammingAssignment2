## The function makecacheMatrix takes a matrix and creates a inverse of 
## the matrix and then caches it
## 

## Takes a matrix as input and and stores it

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        	x <<- y
        	m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function returns the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
        	message("Getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
