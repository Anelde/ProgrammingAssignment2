## The functions in this file will calculate the inverse of a matrix. 
# But will only do that if this calculation has nog been done before.
# If so, it will get the inverse matrix out of the cache memory

## This function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
# created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and 
# sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

## sample output
# > source('~/Coursera/R/ProgrammingAssignment2/cachematrix.R')
# > t <- matrix(sample.int(10,4,replace=TRUE),2,2)
# > > f <- makeCacheMatrix(t)
# > f$get()
#       [,1] [,2]
# [1,]    3    6
# [2,]    8    5
# > cacheSolve(f)
#            [,1]        [,2]
# [1,] -0.1515152  0.18181818
# [2,]  0.2424242 -0.09090909
# > cacheSolve(f)
# getting cached data
#            [,1]        [,2]
# [1,] -0.1515152  0.18181818
# [2,]  0.2424242 -0.09090909