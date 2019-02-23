## The purpose of this R script and its 2 functions is to cache the inverse of a matrix. By doing
## so the cached data can be available for repeated use without having to incur the heavy cost of
## doing the inversion calcutation everytime it is needed.

## Function Description
## The makeCacheMatrix is designed to build a set of functions and then returns those functions 
## contained within a list. The list is returned to the parent environment. makeCacheMatrix 
## contains a complete copy of the environment makeCacheMatrix(). The four functions that are 
## returned to the parent are set(), get(), setsolve(), getsolve(). Also included are 2 data
## objects 'x' and 's' ('s' is for 'solve' whereas the example used 'm' for 'mean'. )

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
        
}


## Function Description
## The cacheSolve function is required to populate and/or retrieve the matrix inverse from an 
## object of type makeCacheMatrix(). cacheSolve is the only place where the solve() function
## gets executed. If the value for 's' is not null then there is a valid, cached inverted matrix 
## that can be returned to the parent environment. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s        
        
}

## Test Cases
## Provides for 3 test cases to test the functions. All test cases attempt to run the routine 
## a 2nd time to determine of the data is retrieved from cache otherwise it will be recreated
## then cached.

s1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(s1)
cacheSolve(myMatrix_object)

cacheSolve(myMatrix_object)


s2 <- matrix( c(1,0,4,1,3,4,4,1,0), nrow=3, ncol=3)
myMatrix_object <- makeCacheMatrix(s2)
cacheSolve(myMatrix_object)

cacheSolve(myMatrix_object)



s3 <- matrix (c(1,2,3,4), nrow =2, ncol =2)
myMatrix_object <- makeCacheMatrix(s3)
cacheSolve(myMatrix_object)

cacheSolve(myMatrix_object)