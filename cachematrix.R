## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 		#Stores the cached inverse of matrix
	set <- function(y)     #Setter for the matrix
	{
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse  
	getinverse <- function() inv
	#return the matrix with newly defined functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
				 ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
	if(!is.NULL(inv))   	 #if the inverse is already calculated, then return inverse of the matrix
	{
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)   #calculating the inverse of the matrix
	x$setinverse(inv	  #cache the inverse of the matrix
	inv  			  #return the inverse
}
