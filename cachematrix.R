## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This assignment will focus on creating two functions that cache the inverse of a matrix and retrieve the cache if the matrix has not changed

## The first function, makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL                           ##Set the inverse to NULL to store a future value
        
        set <- function(y) {                  ##Sets matrix x to a new matrix y and resets inv
                x <<- y                       ##to NULL.
                inv <<- NULL
        }
        
        get <- function() x                   ##Returns the matrix x
        
        setinverse <- function(inverse) inv <<- inverse          ##Sets inverse, inv, to inverse
        
        getinverse <- function() inv                             ##Returns the inverse, inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ##List containing all calculated functions in 'makeCacheMatrix'

}


## The following function, cacheSolve, to calculate the inverse of a matrix. If the inverse of the matrix exists in cache,
## the function retrieves the inverse of the matrix from cache. If it doesn't exist, it calculates the inverse 
## and sets the inverse value in cache.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {     
        inv <- x$getinverse()                      ##Retrieves function the inverse of the matrix from cache using getinverse() function.
        
        if(!is.null(inv)) {                         ##If inv is not NULL
          message("getting cached data")           ##Returns message that inverse is being retrieved from cache
          return(inv)                              ##Returns inverse from function
        }
        data <- x$get()                            ##If inverse was not in cache, 
        
        inv <- solve(data)                         ##Calculate inverse of matrix x using 'solve'
        
        x$setinverse(inv)                          ##Sets the inverse value in cache using setinverse()
        
        inv                                        ##Prints inverse of matrix, inv
}

##Sample Run
##> x <- rbind(c(2, 1), c(1, 2))
##> m <- makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    2    1
##[2,]    1    2
##First time run with no data in cache
##> cacheSolve(m)
##[,1]       [,2]
##[1,]  0.6666667 -0.3333333
##[2,] -0.3333333  0.6666667
##Second run with data in cache
##> cacheSolve(m)
##getting cached data
##[,1]       [,2]
##[1,]  0.6666667 -0.3333333
##[2,] -0.3333333  0.6666667
