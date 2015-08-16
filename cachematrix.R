## Put comments here that give an overall description of what your
## functions do

## Function cacheSolve creates inverse matrix of the original input matrix. But before 
## calculating one, it checks for cached inverse matrixes. If exists, message is printed and cached 
## matrixes would be returned, otherwise inverse matrix is calculated using solve()

## for example:
##  > m <- makeCacheMatrix(matrix(c(1,2,3,4), c(2,2)))
##  > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 

## Write a short comment describing this function

## 1. set value of matrix
## 2. get maatrix
## 3. set inverse matrix
## 4. get inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function

## get inverse matrix of input matrix x.
## if inverse matrix is already cached, message is displayed and inverse matrix is returned
## if inverse matrix is not cached, then inverse matrix is calculated and returned

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
