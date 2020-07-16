## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" that can cache its invert

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(solveMatrix) inv <<- solveMatrix
	getInverse <- function() inv
	list (set= set, get = get, setInvers = setInverse, getInverse = getInverse)
}


## this function computes the inverse of the "matrix" returned by the makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
