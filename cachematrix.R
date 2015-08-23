
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# The output as follows :

> x = rbind(c(1, 123,567), c(2,456, 654), c(3,789, 348))
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2] [,3]
#[1,]    1  123  567
#[2,]    2  456  654
#[3,]    3  789  348
#> cacheSolve(m)
#[,1]         [,2]         [,3]
#[1,]  4.329552890 -4.901962923  2.158124318
#[2,] -0.015339876  0.016394039 -0.005816067
#[3,] -0.002544529  0.005089059 -0.002544529
#> cacheSolve(m)
#getting cached data
#[,1]         [,2]         [,3]
#[1,]  4.329552890 -4.901962923  2.158124318
#[2,] -0.015339876  0.016394039 -0.005816067
#[3,] -0.002544529  0.005089059 -0.002544529
#>


