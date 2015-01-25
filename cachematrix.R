## The following functions chache the inverse of a matrix    
## We assume we're working with invertible matrix


## makeCacheMatrix takes a matrix as an argument and creates a special "matrix",
## that can cache its inverse. 
## The special matrix returned is a list, containing the following methods:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x=matrix( , )){
	i <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The function cacheSolve computes the inverse of the special "matrix" returned by the makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieve the inverse from the cache.


cacheSolve <- function(x){
	i <- x$getInverse()
	if(!is.null(i)){
		message("getting cached inverse")
		return(i)
	}
	data <- x$get()
	print(data)
	i <- solve(data)
	x$setInverse(i)
	i
}
