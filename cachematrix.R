## Caching the Inverse of a Matrix
## Programming Assignment #2 in R Programming:version1.0

## Two functions are included: "makeCacheMatrix" and "cacheSolve".
## "cacheSolve" is based on "makeCacheMatrix".


## This function "makeCacheMatrix" creates a list of four functions
## as list(set, get, setInverse, getInverse), aiming at set or get 
## the value of a matrix and its inverse matrix respectively.
## The only argument should be an invertible matrix.

## Example:
##    a<- makeCacheMatrix(matrix(1:4,2,2))
##    a$get()
##	a$setInverse(solve(matrix(1:4,2,2)))

makeCacheMatrix <- function(x = matrix()) {
	invx<-NULL
	set<-function(y){
		x<<-y
		invx<<-NULL
	}
	get<-function()x
	setinv<- function(z) invx<<-z
	getinv<- function() invx
	
	list(set = set, get = get,
             setInverse = setinv,
             getInverse = getinv)

}



## This function "cacheSolve" computes the inverse of a MATRIX. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

## The only argument MUST be the list created by makeCacheMatrix.

## Example:
## 	x<-makeCacheMatrix(matrix(1:4,2,2))
##	cacheSolve(x)

cacheSolve <- function(x) {
         invx <- x$getInverse()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data)
        x$setInverse(invx)
        invx
}
