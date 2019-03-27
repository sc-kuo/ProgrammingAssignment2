## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a "matrix" to cache a matrix's inverse later on.

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<-function(y){
            x<<-y
            inv<<-NULL
      }
      get<-function()x
      setInverse<-function(solveMatrix) inv<<-solveMatrix
      getInverse<-function()inv
      list(set=set,get=get,setInver=setInverse,getInverse=getInverse)
}


## The CacheSolve function checks if the inverse of a matrix is already stored in the cashe.
## Otherwise, it solves for the inverse of a matrix and stores it in the cache, and displays the inverse as well.

cacheSolve <- function(x, ...) {
      inv<-x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data<-x$get()
      inv<-solve(data)
      x$setInverse(inv)
      inv
        ## Return a matrix that is the inverse of 'x'
}
