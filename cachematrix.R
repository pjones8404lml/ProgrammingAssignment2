## Matrix Inversion, greater than a 3 by 3 are labor intenisve.
## Even inverting a 20x20 matrix is time consuming on a x86 computer.
## About 90% of all analysis is done using traditional Statistics Methodology
## At its core, all Linear Modeling is Martrix Model
## In Order to do this, you will almost always need to invert a martix.
## This function allows for you to invert a Square matrix once, store it, and 
## then reuse it over and over.

## makeCacheMatrix is a funtion that will invert a large matrix
## Store it in Memmory for recall
## An interesting use would be to write it to disk

makeCacheMatrix <- function(x = matrix()) {
  # Set Matrix Inverse to NULL
  xinv <- NULL
  #Create function X1 to set matrix Inverse
  set <- function(x1) {
    #Set  X to the Global Environment
    x <<- x1
    #Set  xinv to the Global Environment
    xinv <<- NULL
  }
  get <- function() x
  # Set Inverse 
  setinverse <- function(inverse) xinv <<- inverse
  
  #Get Inverse
  getinverse <- function() xinv
  
  #Set List
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Main FUnction to solve a Matric from the Cached Inverse

cacheSolve <- function(x, ...) {
  #Set xinv to the inverse of X
  xinv <- x$getinverse()
  #Error Check to see if Cache is Null
  if(!is.null(xinv)) {
    # If xinv Null, create Cache
    message("Getting Inverse Martix of X")
    #Else Return the Inverse
    return(xinv)
  }
  #Set Data to Get X
  data <- x$get()
  #Create Solved Data
  xinv <- solve(data)
  #Set Inverse
  x$setinverse(xinv)
  xinv
}

#Test Data using Random 3x3 Matrix- Can be Scaled to nxn
x = matrix(rnorm(9), 3, 3)
m = makeCacheMatrix(x)
m$get()


x = matrix(rnorm(81), 9, 9)
m = makeCacheMatrix(x)
m$get()