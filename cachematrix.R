## Put comments here that give an overall description of what your
## functions do
## These 2 functions work in tandem - one creates a special list of functions,
## another takes it as an argument



##Just like an example makeVector() function, MakeCacheMatrix() creates
## a list containing several functions: 1)set: sets the intial matrix
## 2)get: retrieves the intial matrix 3) setsolved: sets the the inverse matrix
##4) getsolved: gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolved <- function(solved) s <<- solved
        getsolved <- function() s
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)
        
}


## Write a short comment describing this function
## This function takes a list returned by makeCacheMatrix() and checks if the inverse
## matrix ahs already been calculated. If not, it calculates the inverse and sets it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolved()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolved(s)
        s
        
}
