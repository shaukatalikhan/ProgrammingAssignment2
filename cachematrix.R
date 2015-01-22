## I have created this function to work as class ,
# It produces  a list that contains four member functions  :
## set,get,setInv and getInv.
#Interval variables are saved from exposing to outside environment by
# using Assignment Operator
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {


      xinv <- NULL 
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL 
      }

      get <- function() x
      setInv <- function(inv) xinv <<- inv 
      getInv <- function() xinv 
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)

}


## This Function  will create inverse matrix fro object X

cacheSolve <- function(x, ...) {
       
        m <- x$getInv() 
      if(!is.null(m)) {
	  message("getting cached data")
	  return(m) 
      data <- x$get()
      m <- solve(data) 
      x$setInv(m) 
      m 
}

# Test
 
  matrix1 <- matrix(runif(9,1,100),3,3)

  # generate the makeCacheMatrix object with this matrix

  matrixCached <- makeCacheMatrix(matrix1)

  # from now on calculate or retrieve calculated inversion using the cacheSolve function

 matrixInv <- cacheSolve(matrixCached)
