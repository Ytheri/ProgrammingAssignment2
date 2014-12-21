## There are 4 functions within makeCacheMatrix
## setmatrix() to assign the data for the matrix
## getmatrix() to get the data for the matrix
## setinverse() to assign the data for the inverse
## getinverse() to get the data for the inverse


makeCacheMatrix <- function(x = matrix()) { ## x is the matrix
        i <- NULL ## i stores the inverse of the matrix
        setmatrix <- function(y) {
                x <<- y # x is being assigned the matrix y 
                i <<- NULL
        }
        getmatrix <- function() x 
        setinverse <- function(inv) i <<- inv 
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve checks if i already has data
## if i is NULL, it solves the matrix
## after solving setinverse() is called to set the inverse
## if i has data, then i is returned without solving

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse() ## getting i to check
        if(!is.null(i)) {  ## if i is not NULL
                message("Data already exists.\nGetting cached data")
                return(i) ## printing i
        } 
        ## if i is NULL solving matrix
        message("Solving matrix to get inverse")
        data <- x$getmatrix() ## getting original matrix
        i <- solve(data, ...) ## solving matrix
        message("Caching solved data")
        x$setinverse(i) ## setting value of i
        i ## Returning a matrix that is the inverse of 'x'
}
