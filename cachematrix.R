## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
 
makeCacheMatrix <- function(x = matrix()) {
	iv<-NULL
	set <- function (v) {
		 x<<-v
		 iv<<-NULL
    }
    get<-function() x
    invMatrix <- function(solve) iv<<-solve
    getInvMatrix<-function() iv
    list(set=set,get=get,
         invMatrix=invMatrix,getInvMatrix=getInvMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv<-x$getInvMatrix()
        if (!is.null(iv)) {
           return (iv)
        }
        data<-x$get()
        iv<-solve(data, ...)
        x$invMatrix(iv)
        iv
}
