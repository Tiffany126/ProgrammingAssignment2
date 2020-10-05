## This pair of functions will cache the inverse of a matrix
## which helps reduce the work to compute the inverse of matrix


## This function creates a special "list" object that
## can cache its inverse.
## This function creates a special list, which contains:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) compute the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
              invs<-NULL ##invs stands for 'inverse'
              set<-function(y){
              x<<-y
              invs<<-NULL
 }
 get<-function() x
 compute_inverse <-function(inverse) invs<<-inverse
 get_inverse<-function () invs
 list(set = set, get = get,
      compute_inverse = compute_inverse,
      get_inverse = get_inverse)
}



## The following function calculates the inverse of a matrix.
## However, it first checks to see if the inverse has been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwis, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        invs<-x$get_inverse() 
        if(!is.null(invs)){
          message("already got cached data")
          return(invs)
        } ##check whether or not the inverse of matrix exist
        data <- x$get() #get the matrix
        invs<-solve(data)
        x$compute_inverse(invs)
        invs
}
