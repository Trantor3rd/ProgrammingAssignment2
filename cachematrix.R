## Matrix inversion cache programming assignment #2.


## Create a list of functions that cacheSolve will use.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        print(environment())
        evn <- environment()
        print(parent.env(evn))
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        getevn<- function() environment()
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv,
             getevn = getevn)
}


## Return a cached inverse matrix if m exists in cache else compute inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        

        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m    
        
}
