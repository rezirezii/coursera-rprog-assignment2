## This function create a special matrix which is naturally a square invertable matrix and cash inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  matrix_inverse <- NULL                      ## initialize value of matrix inverse to NULL
  set <- function(y) {                 ## set value of matrix
    x <<- y                                 ## if matrix is changed value of inverse is changed
    matrix_inverse <<- NULL                 ## matrix inverse is NULL if used cashSolve
  }
  get <- function() x                         ## gets inverse value 
  setinverse <- function(inverse) matrix_inverse <<- inverse ## compute inverse by inverse function
  getinverse <- function() matrix_inverse                ## gets inverse 
  list(set=set,                 ## passes value of makeCacheMatrix function
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)                       
}



# This function computes the inverse of special matrix returned bythe above (makeCasheMatrix). If matix has not changed and alredy computed the inverse return from cacheSolve.

matrix_inverse  <- x$getinverse()   # get inverse of matrix if exists
if(!is.null(matrix_inverse)) {      # check cacheSolve has already been run or not
  message("getting cached data, inverse of matrix exists.")
  return(matrix_inverse)
}
matrix_data <- x$get()          #if there is no inverse, it is computed
matrix_inverse  <- solve(matrix_data)
x$setinverse( matrix_inverse )
matrix_inverse 
}
## ---------------Example------------------------
## mat <- matrix(data = c(1,6,3,4,2,7,6,9,10), nrow = 3, ncol=3)
## mat1 <- makeCacheMatrix(mat)
## cacheSolve(mat1)
##           [,1]        [,2]       [,3]
## [1,] -1.0487805  0.04878049  0.5853659
## [2,] -0.8048780 -0.19512195  0.6585366
## [3,]  0.8780488  0.12195122 -0.5365854
## cacheSolve(mat1)
## getting cached data, inverse of matrix exists.
[,1]        [,2]       [,3]
## [1,] -1.0487805  0.04878049  0.5853659
## [2,] -0.8048780 -0.19512195  0.6585366
## [3,]  0.8780488  0.12195122 -0.5365854

