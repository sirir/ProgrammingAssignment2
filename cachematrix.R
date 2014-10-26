## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # create 'inv' variable to store inverse of a mtrix
        inv <- NULL

        # nullify the cache
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        #'get' gets the row matrix
        get <- function() x

        #'setinv' set the inverse variable
        setinv <- function(i) inv <<- i

        #'getinv' gets the cahced inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #get cached inverse
        inv <- x$getinv()
        if(!is.null(inv)) {
                #if the inverse already cached, we get result from here and the below message will be displayed
                message("getting cached data")
                return(inv)
        }
        matr <- x$get()
        #claculate the inverse of the given matrix
        inv <- solve(matr, ...)
        x$setinv(inv)
        inv

}
