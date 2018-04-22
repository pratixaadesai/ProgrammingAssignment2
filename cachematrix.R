## programming assignment week 3 

## This function creates a special "matrix" object 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<-y
    m <<-NULL
  }
  
  get <- function()x
  setinverse <- function (inv)m <<- inv
  getinverse<-function()m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Return a matrix that is the inverse of 'x'

cacheinverse <-function(x, ...){
  m<- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## examples 

a<-matrix(c(8,4,3,2), nrow=2)
b<-matrix(c(9,3,2,5), nrow=2)
cacheinverse(makeCacheMatrix(a))
cacheinverse(makeCacheMatrix(b))
cacheinverse(makeCacheMatrix(b))
