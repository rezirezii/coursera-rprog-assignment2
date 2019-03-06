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
 
 
 
 # This function assumes that the matrix is always invertible.
 cacheSolve <- function(x, ...) {
     matrix_inverse  <- x$getinverse()   # get inverse of matrix if exists
     if(!is.null(matrix_inverse)) {      # check cacheSolve has already been run or not
            message("getting cached data, inverse of matrix.")
           return(matrix_inverse)
     }
     matrix_data <- x$get()          #if there is no inverse, it is computed
     matrix_inverse  <- solve(matrix_data)
     x$setinverse( matrix_inverse )
     matrix_inverse 
 }
  