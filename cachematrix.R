## Programming Assignemnet 2: Lexical Scoping

## Caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of a matrix returned by function 'makeCachematrix'.
## If the inverse has already been calculated it will return the cached value.

cacheSolve <- function(x, ...) {
        calc <- x$getinv()
        if(!is.null(calc)) {
                message("getting cached data")
                return(calc)
        }
        data <- x$get()
        calc <- solve(data, ...)
        x$setinv(calc)
        calc
}
