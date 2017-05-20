## Comments: These two R functions were created for Assignment n° 2 of
## R Programming course. They are used to create a matrix and caches its
## inverse (usually a costly computation).

## The following "makeCacheMatrix" function creates a matrix that can
## cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        SetI <- function(mtxinv) I <<- mtxinv
        GetI <- function() I
        
        list(set = set,
             get = get,
             SetI = SetI,
             GetI = GetI)
}


## The following "cacheSolve" function retrieves the inverse of the matrix
## that "makeCacheMatrix" function creates. If the inverse has been computed
## previously, the function retrieves the inverse from the cache, instead of
## recalculating it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$GetI()
        if (!is.null(I)) {
                message("getting cached value")
                return(I)
        }
        mtx <- x$get()
        I <- solve(mtx, ...)
        x$SetI(I)
        I
}
