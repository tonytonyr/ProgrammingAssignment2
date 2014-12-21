## Functions for caching the results of matrix inversion
## caches the results of solve(m) where m is created via makeCacheMatrix()
## and inverse is calculated / retrieved via cacheSolve(m)

## makeCacheMatrix - create cachable matrix structure

makeCacheMatrix <- function(m = matrix()) {
        I <- NULL

        set <- function(y) {
                m <<- y
                I <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse
             )
}


## cacheSolve - retrieves cached results inverse or performs solve(m)
## and caches the results

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        I <- m$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        ## Cache the inverse of 'm' for future retrival
        data <- m$get()
        I <- solve(data, ...)
        m$setinverse(I)
}


