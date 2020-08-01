## This functions are used to create a special object that stores a invertable matrix and cache's its inverse

## makeCacheMatrix is a function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<-inverse
        getinv <-function() inv
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. Otherwise, calculated the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
