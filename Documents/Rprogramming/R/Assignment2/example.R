makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

##The makeVector function creates a  a "vector" - a list containing functions to
##set(y) - set the value of the vector
##get(x) - get the value of the vector
##setmean(m) - set the value of the mean
##getmean(m) - get the value of the mean

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

##function calculates the mean of the special "vector" created with the above function. 
##However, it first checks to see if the mean has already been calculated. 
##If so, it gets the mean from the cache and skips the computation. 
##Otherwise, it calculates the mean of the data and sets the value of the mean in the cache 
##via the setmean function.