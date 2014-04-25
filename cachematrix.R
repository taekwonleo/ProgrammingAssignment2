makeCacheMatrix <- function(x = matrix()) {              
        inm <- NULL                                     
        set <- function(y) {                                     
                x <<- y                                     
                inm <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inm <<- solve
        getinverse <- function() inm
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   
}

cacheSolve <- function(x, ...) {
        inm <- x$getinverse()
        if(!is.null(inm)) {
                message("getting cached inverse of matrix")
                return(inm)
        }
        data <- x$get()
        inm <- solve(data, ...)
        x$setmean(inm)
        inm
}