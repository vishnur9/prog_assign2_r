# prog_assign2_r
#programming assignment 2 


#makeCacheMatrix is a function which creates a special matrix and caches its inverse
#it has a list of functions to set the values of a matrix,get the values of a matrix,set the values of the inverse matrix
#and also to get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #1.function to set the values in a matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #2.function to get the values of a matrix
  get <- function() x
  #3.function to set the inverse matrix once the inverse is calculated so that the inverse matrix is 
  #stored in the cache and can be used again if needed 
  setinv <- function(inverse) i <<- inverse
  #4.function to get the inverse matrix that is present in the cache
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



#function to calculate the inverse matrix and also to set the inverse matrix in the cache
#it basically checks for the result in the cache, if present, the inverse matrix is returned
#else the inverse matrix is calculated and is stored in the cache 
cacheSolve <- function(x, ...) {
  #checking if the inverse matrix exits in the cache via getinv() method
  i <- x$getinv()
  if(!is.null(i)) {
    #if present,the inverse matrix is printed
    message("getting cached data")
    return(i)
  }
  #if the inverse matrix is not calculated, it is calculated via solve() method and
  #then stored in the cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  #the inverse matrix after calculated is printed
  i
}

#note: make sure the matrix given as an input is a non-singular matrix.

#sample output:

#creation of a matrix object

# >x<-matrix(c(2,1,1,-5,-3,0,1,1,-1),nrow=3,ncol=3,byrow=T)

#calling makeCacheMatrix function on the created matrix object

# > m = makeCacheMatrix(x)

#calling cacheSolve function for the above result
#since, the inverse is calculated for the first time, it is not present in the cache

# > cacheSolve(m)
#       [,1] [,2] [,3]
# [1,]   -3   -2   -3
# [2,]    5    3    5
# [3,]    2    1    1

#second call on the same object, so the matrix present in the cache is returned
# > cacheSolve(m)
# getting cached data

#       [,1]  [,2] [,3]
# [1,]   -3   -2   -3
# [2,]    5    3    5
# [3,]    2    1    1
