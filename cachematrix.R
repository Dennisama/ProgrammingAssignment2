## Put comments here that give an overall description of what your
## functions do

## Creating a special "vector", which is a list containing set and get functions

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y) {
              x<<-y
        inverse<<-NULL  
        }
        get<-function() x
        setinverse<-function(inv) inverse<<-inv
        getinverse<-function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calulating the inverse of matrix, getting the result from the cache if necessary

cacheSolve <- function(x, ...) {
         inverse <- x$getinverse()
         if(!is.null(inverse)){
                    message("getting cached data")
                    return(inverse)
         }
         result <- x$get()
         inverse <- solve(result, ...)
         x$setinverse(inverse)
         inverse
}
