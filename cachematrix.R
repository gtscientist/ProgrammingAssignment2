## These functions can be used to cache the inverse of a matrix so it can be re-used instead of having to be
## computed repeatedly (computing the inverse can be a costly operation).
## makeCacheMatrix creates a special type of matrix object where you can then use the cacheSolve function to
## compute and store the inverse within the makeCacheMatrix object.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## i will store the inverse; set to NULL initially so that (e.g.) myMatrix$getinverse wouldn't 
        ## throw an error if the inverse hasn't yet been computed
        i <- NULL  
        
        ## The set function takes a matrix (although not stated directly in the function formals) and
        ## assigns that matrix to the object x in the parent environment (i.e., the makeCacheMatrix environment)
        ## and also sets the inverse i (also in the parent environment) to NULL.
        ## That's what the <<- operator does: assign the value on the right of the operator to an object 
        ## in the parent environment named by the object on the left side of the operator. Lexical Scoping in R
        ## is what allows you to write functions in this way.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## The get function just returns the matrix x (from the parent environment since x doesn't exist within get)
        get <- function() x
        
        ## The setinverse function takes a matrix (although not stated directly in the function formals) and
        ## assigns that matrix to the object i in the parent environment (i.e., the makeCacheMatrix environment)
        ## See notes regarding the <<- operator in the set function comments above.
        setinverse <- function(inv) i <<- inv
        
        ## The getinverse function just returns i (from the parent environment since i doesn't exist within getinverse)
        getinverse <- function() i
        
        ## Return a list of getters and setters; they are named so that we can use the $ extract 
        ## operator to access the functions (instead of the [[ bracket notation)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 'makeCacheMatrix'. If the inverse has
## already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Get existing inverse (note that it could be NULL)
        i <- x$getinverse()
        
        ## If the current inverse is not null, then get the cached value...
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        
        ## ...otherwise compute the inverse by getting the matrix x, using solve() to find the inverse, 
        ## and setting the inverse back in the special makeCacheMatrix object (which was passed as an argument). 
        ## This ability to get and set values within the parent object is the magic of lexical scoping.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Example data for testing
##
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
##
## myMatrix_object <- makeCacheMatrix(m1)
##
## and obtain its inverse by
##
## cacheSolve(myMatrix_object)
##      which should return:
##           [,1] [,2]
##      [1,]    6    8
##      [2,]    2    4
## 
## Calling cacheSolve again should retrieve (not recalculate) the inverse from the cache
## 
## You can use the set function to "put in" a new matrix.
##
## n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
##
## myMatrix_object$set(n2)
##
## and obtain its matrix inverse by
##
## cacheSolve(myMatrix_object)
##      which should return:
##              [,1] [,2]
##      [1,]    3    7
##      [2,]    1    5
##
##
##
##