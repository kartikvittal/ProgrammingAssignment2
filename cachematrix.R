#function to create the makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Setting this as NULL so that cacheSolve knows 
            #if it has to calculate or print existing value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #This is a function to get the matrix value in cacheSolve function
  get <- function() 
  {x
  }
  #Calling the Solve function (inverse) - SET
  setsolve <- function(solve) 
  {m <<- solve
  }
  #Getting the value
  getsolve <- function() 
  {m
  }
  list(set = set, get = get,
       setsolve = setsolve, getsolve = getsolve)
  
  
}

#Function to return the inverse of the matrix 
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    # if the inverse already exists then just print it 
    message("getting cached data")
    return(m)
  }
  #Else calculate it
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

##Test cases from one of the discussion forums
size <- 2 # size of the matrix edge, don't make this too big
mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
mymatrixinverse <- solve(mymatrix)

specialmatrix   <- makeCacheMatrix(mymatrix)
special.solved.1 <- cacheSolve(specialmatrix)
special.solved.2 <- cacheSolve(specialmatrix)
identical(mymatrixinverse, special.solved.1)
identical(mymatrixinverse, special.solved.2)