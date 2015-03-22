## The following two functions can calculate the inverse of a
#  square invertible matrix and cache the output.
#  In case the user attempts to recalculate the inverse of a matrix which
#  has already been calculated, then the output is retrieved from the cache,
#  so avoiding to waste time.

## The first function originates a list containing four functions,
#  which are described singularly below.
#  The input is a matrix
#  (e.g. output1 <- makeCacheMatrix(matrix(runif(Nrow*Ncol),Nrow,Ncol))).
#  For our goal it has to be a square invertible matrix: Nrow=Ncol.

makeCacheMatrix <- function(x = matrix()) {
  mat<-NULL
  # function to set the value of the matrix
  set <- function(y){
    x<<-y
    mat<<-NULL
  }
  # function to get the value of the matrix
  get<-function() x
  # function to set the value of the matrix inverse
  setinv<- function(inverse) mat<<-inverse
  # function to get the value of the matrix inverse
  getinv <- function() mat
  #creates a list of functions
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## The function calculates the matrix inverse and save the output in
#  the cache, but first check if it has already been calculated. If yes,
#  then the output is retrieved from the cache and displayed and it avoid
#  to compute it again.
#  The input is the output of the previous function
#  (e.g. output2<-cacheSolve(output1))

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Check if the matrix is already in the cache
  # If yes, display the result and skip
  mat<-x$getinv()
  if(!is.null(mat)){
    message("gettingcacheddata")
    print(mat)
    return(mat)
  }
  # otherwise get the input and calculate the matrix inverse
  data<-x$get()
  mat<-solve(data,...)
  # Cache the output
  x$setinv(mat)
  # Display the inverse matrix
  mat
}

## Practical Example
#  Nrow<-4
#  Ncol<-Nrow
#  output1 <- makeCacheMatrix(matrix(runif(Nrow*Ncol),Nrow,Ncol))
#  output2 <- cacheSolve(output1)
#  output2
