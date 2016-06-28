##  A pair of functions that cache the inverse of a matrix.
 

## Creates a list of functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setInverse<-function(inverse) m<<- inverse
        getInverse<-function() m
        list(set=set, get=get, setInverse=setInverse,
             getInverse=getInverse)
}


## Computes the inverse of a matrix unless it has been cached already
## Takes the list of functions created by "makeCacheMatrix" as argument

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
