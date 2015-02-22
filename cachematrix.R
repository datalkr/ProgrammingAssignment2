## makeCacheMatrix - function that creates a special "matrix" 
## object that can cache an inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    #Sets default values if cacheSolve hasn't been used
  y <- NULL
  mset <- function(y) { #Sets value of the matrix
    x <<- y           #cache inputted matrix; cacheSolve checks if it changed
    m <<- NULL
  }
  mget <- function() x
  matrixset <- function(solve) m<<- solve
  matrixget <- function() m
  
  list(mset=mset, mget=mget, matrixset=matrixset, matrixget=matrixget)
}


## cacheSolve - function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function. If the inverse has been
## calcualted, andthe matrix hasn't changed, then the cacheSolve should
## retrieve the inverse from the cache

cacheSolve <- function(x=matrix(),...) {
  m <- x$matrixget()  #get value of input matrix
  if(!is.null(m)) {   #check to see if cacheSolve has been run before
    return(m)
  }
  matrix <- x$mget()
  m <- solve(matrix, ...) #compute value of the inverse of the input matrix
  x$matrixset(m)
  return(m)
}

mat1 <- matrix(data = c(10,4,2,5), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat1)
cacheSolve(mat2)
