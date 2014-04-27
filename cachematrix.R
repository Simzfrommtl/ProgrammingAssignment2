## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.

## MakeCacheMatrix creates a special matrix (as specified by a user) object that 
##can cache its inverse.. 
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(inverse) invMatrix <<- inverse
        getInvMatrix <- function() invMatrix
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

## cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function
#If the matrix inverse has already been calculated, it get it 
#from the cache and print it. Otherwise, it will calculate it and print it

cacheSolve <- function(x, ...) {
        invMatrix <- x$getInvMatrix()
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(invMatrix)) {
                message("getting cached inverse matrix")
                return(invMatrix)
        } else {
        
        invMatrix <- solve(x$get())
        x$setInvMatrix(invMatrix)
        return(invMatrix)
        }
}

