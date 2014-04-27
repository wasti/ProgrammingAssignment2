######
#       These two functions help save computation time by providing ability to cache information.
#       They are design to cache inverse of a square matrix.
   

## Write a short comment describing this function
## First function will create a special "matrix" object that can cache its inverse and 
##   provides access to cached values via getters and setters.
##   usage: input a matrix to the first function e.g: makeCacheMatrix(squareMatrix)). 
##   It will than provide getters and setters for the matrix and inverse Matrix for the caching functions.
# #             

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {            #this section is setting the matrix 
                x <<- y
                m <<- NULL   
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse     #here the inverse value is being passed to cache via setter used by Func cacheSolve.
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

#       Second function actually calculates the inverse of the matrix stored in the first function
#       It does first check if the inverse value of the matrix is already cached or not, if not 
#       than it will calculate the inverse and than store it via setter provided in the first function(cache it) and return the value.
#       usage: cacheSolve(specialMatrixCreatedviaFristFunction)
#       Funciton will return the inverse of the matrix. 
#       If its returning cached vale it will print the message "getting cached data" as well.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()   
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()          # this section calculates the inverse and returns it
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
