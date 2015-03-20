## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
 
makeCacheMatrix <- function(m = matrix()) {
	iv<-NULL
	set <- function (v) {
		 m<<-v
		 iv<<-NULL
    }
    get<-function() m
    setInvMatrix <- function(solve) iv<<-solve
    getInvMatrix<-function() iv
    list(set=set,get=get,
         setInvMatrix=setInvMatrix,getInvMatrix=getInvMatrix)

}

## Write a short comment describing this function

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv<-m$getInvMatrix()
        if (!is.null(iv)) {
           return (iv)
        }
        data<-m$get()
        iv<-solve(data, ...)
        m$setInvMatrix(iv)
        iv
}
