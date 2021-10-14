## Put comments here that give an overall description of what your
## functions do


##The below functions are used to find inverse of a matrix and also cache the inverse so that we can retrieve the inverse from cache in case there are no changes to matrix.This 
## saves computation time.

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <-function(y){
                x <<-y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by the above function.If the inverse is already calculated this function retrieves the inverse from the cache
## instead of computing it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat<- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
}
