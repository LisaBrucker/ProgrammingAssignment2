## Programming Assignment 2: Caching the Inverse of a Matrix
## 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 
## The makeCacheMatrix function stores a list of 4 functions, namely get(),set(),
## setinverse() and getinverse().
## get() gives back the matrix x.
## set() assigns a new matrix to x.
## setinverse () is supposed to give the inverse of the matrix, but it just stores a matrix.
## getinverse () returns the vektor stored in setinverse.
## With the command list the 4 functions are stored in the function makeCacheMatrix.

makeCacheMatrix <- function (x = matrix()) {
        i <- NULL
        set <- function (y = matrix()){
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function (inverse) i <<- inverse
        getinverse <- function () i
        list (set = set, get = get,
              setinverse = setinverse, 
              getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## 
## The cacheSolve funtion first tests if there was an inverse matrix in the function makeCacheMatrix.
## If there was an inverse matrix, then the function displayes the massage that it is getting cached data and returns
## the inverse matrix. If no invers matrix is found than the function creates an inverse matrix and returns this.

cacheSolve <- function (x, ...) {
        i <- x$getinverse ()
        if (!is.null(i)){
                message ("getting cached data")
                return (i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Test
g <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(g)
