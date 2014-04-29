
## Function creates a special "matrix" object that can used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ord <- NULL
    set <- function(y) {
        x <<- y
        ord <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) ord <<-inverse
    getinverse <- function() ord
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function computes the inverse of the special "matrix" returned by above Function.

cacheSolve <- function(x, ...) {
       ord <- x$getinverse()
    if (!is.null(ord)) {
        message("get cached inv matrix")
        return(ord)
    } else {
        ord <- solve(x$get())
        x$setinverse(ord)
        return(ord)
    }
}
