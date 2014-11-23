
## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.
## The cacheSolve function  computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mat_inv <- NULL
      ## "mat_inv" is created to cache the inverse
	set <- function(y) {
                x <<- y
                mat_inv <<- NULL
	}	
	get <- function() x
	setinv <- function(cal_inv) mat_inv <<- cal_inv  
	getinv <- function() mat_inv
	list(set=set, get=get,
		setinv = setinv,
		getinv = getinv)       
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	mat_inv <- x$getinv()
	if(!is.null(mat_inv)) {
		message("getting cached inverse")
		return(mat_inv)
      ## If the inverse has already been calculated (and the matrix has not changed),
      ## just retrieve the inverse from the cache.
	}
	data <- x$get()
	mat_inv <- solve(data,...)
	x$setinv(mat_inv)
      ## If the inverse has NOT been calculated, computes the inverse 
      ## returned by makeCacheMatrix above.
	mat_inv
      ## Return a matrix that is the inverse of 'x'
}










