## 2 functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix: get and set the matrix, get and set invertible matrix
## cacheSolve: check whether there is invertible matrix or not, if not calculate it inverse


makeCacheMatrix <- function(x = matrix()) {
	
	## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()

	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get= get, setinv = setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	# @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'

	inv = x$getinv()

	#if the inverse has already been calculated 
	if (!is.null(inv)){
		#get it from the cache and skips the computation.
		message("getting cached data")
		return(inv)
	}

	#otherwise, calculates the inverse
	mat.data = x$get()
	inv = solve(mat.data, ...)
	
	x$setinv(inv)

	return(inv)

}
