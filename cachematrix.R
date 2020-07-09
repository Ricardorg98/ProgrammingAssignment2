 ##let's make the makeCacheMatrix and cacheSolve function true
> makeCacheMatrix <- function(x = matrix()){
+         inv <- NULL
+         set <- function(y){
+                 x <<- y
+                 inv <<- NULL
+         }
+         get <- function() {x}
+         setInverse <- function(inverse) {inv <<- inverse}
+         getInverse <- function() {inv}
+         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
+ }
> cacheSolve <- function(x, ...){
+         inv <- x$getInverse()
+         if(!is.null(inv)){
+                 message("getting cached data")
+                 return(inv)
+         }
+         mat <- x$get()
+         inv <- solve(mat, ...)
+         x$setInverse(inv)
+         inv
+ }
> ## forming a 4x4 matrix that goes from 1 to 16, using the following code:
> source("makeCacheMatrix.R")  
> matrix<- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
> ##See the matrix:
> matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> ##And now Inverse Matrix:
> matrix$getInverse()
NULL
> cacheSolve(matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
