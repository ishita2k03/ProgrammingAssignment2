## makeCacheMatrix will return a list containing 4 functions, set(), get(), setinverse() and getinverse()
## in function space, it holds matrix x and inverse inv_x
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,setmean = setmean,getmean = getmean)
}

## cacheSolve will return the inverse matrix inv_x and alse set the inv_x held in makeCacheMatrix
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
