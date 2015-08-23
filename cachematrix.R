# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function() {
    data <- NULL
    data_inverted <- NULL

    get <- function() {
        data
    }

    set <- function(x) {
        data <<- x
        data_inverted <<- NULL
    }

    get_inverted <- function() {
        data_inverted
    }

    set_inverted <- function(x_inverted) {
        data_inverted <<- x_inverted
    }

    list(set = set, get = get,
         set_inverted = set_inverted,
         get_inverted = get_inverted)
}

# Computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    data_inverted <- x$get_inverted()
    if(!is.null(data_inverted)) {
        message("Inverted matrix already calculated")
        return(data_inverted)
    }

    data <- x$get()
    message("Calculating inverted matrix")
    data_inverted <- solve(data, ...)
    x$set_inverted(data_inverted)
    data_inverted
}

# Test case
m_cacher <- makeCacheMatrix()
m_cacher$set(matrix(rnorm(9), 3, 3))
m_inverted <- cacheSolve(m_cacher) # Should print "Calculating inverted matrix"
m_inverted <- cacheSolve(m_cacher) # Should print "Inverted matrix already calculated"
print(m_cacher$get())
print(m_inverted)

