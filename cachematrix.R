

##Create the special Matrix object, which can cache the inverse version of itself.
## There are four methods:
## set: set new value for matrix
## get: get Matrix value
## set: set new value for inverse matrix
## set: get Inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
                
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

##Ñalculates the inverse of the matrix.

##If the matrix inverse has already been calculated, it will instead

##find it in the cache and return it, and not calculate it again.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
