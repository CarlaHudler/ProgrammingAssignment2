## Put comments here that give an overall description of what your
## functions do
## The following functions ("makeCacheMatrix" and "cacheSolve") 
## will create an special matrix object that calculates the inverse 
## of the matrix, stores it and retrieves it from the cache rather 
##than computing it repeatedly.

## Write a short comment describing this function
## The function "makeCacheMatrix" creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function()x
setinverse<-function(inverse) inv<<-inverse
getinverse<-function() inv
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## The function "cacheSolve" computes the inverse 
## of the matrix returnet by "makeCacheMatrix". 
## As long as the function used in "makeCacheMatrix" 
## has not changed and the inverse of the array has 
## been computed, we expect "cacheSolve" to retrieve 
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv<-x$getinverse()
if(!is.null(inv)){
  message("getting inverse matrix")
  return(inv)
}
data<-x$get()
inv<-solve(data,...)
x$setinverse(inv)
inv
}
