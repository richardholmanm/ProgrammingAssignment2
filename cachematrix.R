# The functions written below create a new matrix and also 
# other function to edit them

## this function create a new function with a specified amount of numbers, 
## and with specified dimension

makeCacheMatrix <- function (x=matrix()){
  inv <- NULL
  set <- function (y){
    x<<- y
    inv <<- NULL
  }
  get <- function () {x}
  setInverse <- function (inverse) {inv <<- inverse}
  getInverse <- function () {inv}
  list (set = set, get=get, setInverse = setInverse, getInverse=getInverse)
}


## this function enables to call the inversed matrix create by the
## makeCacheMatrix created above

cacheSolve <- function (x, ...){
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  mat<-x$get()
  inv <-solve(mat, ...)
  x$setInverse(inv)
  inv
}