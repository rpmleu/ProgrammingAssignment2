## The functions create an object that stores the matrix, calculate its inverse
## matrix and cache it.  And return the cache value.


## The makeCacheMatrix function creates an R object that stores the "special" 
## matrix and its inverse.  It initializes the variable m to Null and 
## define the 4 function sets : set, get, setsolve and getsolve. The object m
## will later cache the inverse of the matrix x. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  ## initialize m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The cacheSolve function computes the inverse of the "matrix" returned by the
## makeCacheMatrix function if it has not been computed and cache already. If 
## the inverse has already been calculated and the matrix has not changed,
## then this function should retrieve the inverse from the cache value stored 
## in the makeCacheMatrix environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

