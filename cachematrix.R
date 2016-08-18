## Programming Assignemnet 2: Lexical Scoping

## Caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # initialise i
        set <- function(y) { # set x and delete i if an inverse was already set
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv # set inverse value
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of a matrix returned by function 'makeCachematrix'.
## If the inverse has already been calculated it will return the cached value.

cacheSolve <- function(x, ...) {
        calc <- x$getinv()
        if(!is.null(calc)) {  # check if cached version exists and return value if not NULL
                message("getting cached data")
                return(calc)
        }
        data <- x$get()
        calc <- solve(data, ...) # Calculate inverse if not in cache
        x$setinv(calc)
        calc
}
