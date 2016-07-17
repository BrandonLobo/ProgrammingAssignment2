## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## As in the example the function has 4 sections
## Set: To set the values of the matrix and it's inverse outside of the local
## environment of the function
## Get: it fetches the value of the matrix to be used for calculation
## Setinv: sets the value of the calculated inverse matrix outside the local
## environment of the function
## Getinv: It is used to fetch the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invmat<-NULL
  set<-function(y){
    x<<-y
    invmat<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)invmat<<-inverse
  getinv<-function()invmat
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## The Cachesolve function here accepts the matrix who's inverse needs to be found
## We then proceed to fetch the inverse of the matrix stored in the global 
## environment, if it is not null then we return the matrix as the cached inverse
## If the value of the cached inverse is null then we calculate the inverse of 
## of the matrix and we cache it for future use and return the inverse for use.

cacheSolve <- function(x, ...) {
  invmat<-x$getinv()
  if(!is.null(invmat)){
    message("Getting Cached Data")
    return(invmat)
  }
  mat.data<-x$get()
  invmat<-solve(mat.data,...)
  x$setinv(invmat)
  return(invmat)
}