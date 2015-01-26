## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  if("MASS" %in% rownames(installed.packages()) == FALSE){
    message("Install MASS....")
    install.packages("MASS")
    library("MASS")
  }
  
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverseMatrix <<- inv
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
  if("MASS" %in% rownames(installed.packages()) == FALSE){
    message("Install MASS.....")
    install.packages("MASS")
    library("MASS")
  }  
  
  invMx <- x$getInverse()
  
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  data <- x$get()
  invMx <- ginv(data)
  x$setInverse(inverseMatrix)
  
  inverseMatrix
}
