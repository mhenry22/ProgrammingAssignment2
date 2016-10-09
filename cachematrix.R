## As it is written in the course assignment, matrix inversion is usually a 
## costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly. The aim here is to write a pair 
## (makeCacheMatrix(), cacheSolve()) of functions that caches the inverse of a 
## matrix so that, if this inverse has already been computed and if the matrix 
## has not changed, R doesn't need to compute it again and can just get it 
## immediately from the cache. We assume that all the matrices supplied are 
## invertible.


## The first function, makeCacheMatrix(), creates a special "matrix" object that
## can cache its inverse. It takes a matrix as its only argument and really 
## creates a list containing functions to set the value of the matrix, get it, 
## set the value of the inverse of the matrix and finally get its value.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function(){
        x
    }
    setinverse <- function(inverse){
        inv <<- inverse
    }
    getinverse <- function(){
        inv
    }
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## The second function, cacheSolve(), computes the inverse of the special 
## "matrix" returned by makeCacheMatrix() above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve() function 
## should retrieve the inverse from the cache. Thus, the function begins by
## checking if the wanted inverse has already been calculated and if so, it 
## stops and just gets ths inverse from the cache. Otherwise, it computes the
## inverse and put it in the cache through the setinverse() function before 
## returning the result.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    else{    
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        return(inv)
    }
}