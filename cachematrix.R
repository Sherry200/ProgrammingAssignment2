## This functions create a special "matrix" object 
## that can cache its m_inverse. 
## If the m_inverse has already been calculated 
## then cacheSolve should retrieve the m_inverse from the cache.

## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    set <- function(y){
         x <<- y
         m_inverse <<- NULL
    }
    get <- function() x
    setinverse<- function(solve) m_inverse <<- solve
    getinverse <- function() m_inverse
    list(set = set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the m_inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the m_inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve should retrieve the m_inverse from the cache.
## Assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the m_inverse of 'x'
      m_inverse <- x$getinverse()
      if(!is.null(m_inverse)){
            message("getting cached data")
            return(m_inverse)
      }
      data <- x$get()
      m_inverse <- solve(data, ...)
      x$setinverse(m_inverse)
      m_inverse
}

