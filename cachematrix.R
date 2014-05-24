## makeCacheMatrix and cacheSolve are a pair of functions that compute and 
##	cache the inverse of a matrix.
## If the inverse of a matrix has already been computed and stored in the cache, 
## 	then the functions return the inverse from the cache rather than computing it. 

## makeCacheMatrix creates a list containing functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {					##1
		x <<- y
		i <<- NULL
		}
	get <- function() x					##2
	setinverse <- function(solve) i <<- solve		##3
	getinverse <- function() i				##4
	list(set = set, gfet = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, 
## 	then cacheSolve retrieves the inverse from the cache.
## If the inverse has not already been calculated then it calculates the inverse.

cacheSolve <- function(x, ...) {
	i <- x$getinverse() 				##Get cached data for the inverse of matrix 'x' 
	if(!is.null(i)){					##	if it is in the cache.	
		message("getting cached data")	
		return(i)
	}
	data <- x$get()					##Otherwise get the matrix 'x', 
	i <- solve(data,...)				## compute the inverse and store it in the cache
	x$setinverse(i)
	i
}
##}
