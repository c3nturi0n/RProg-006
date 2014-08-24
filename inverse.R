# RProg Assignment 2

makeMatrix <- function(x = matrix()) {  
        m <- NULL                # m will store the matrix until function is called. Then it'll be stored as NULL
        set <- function(y) {     # takes input vector
                x <<- y          # saves input vector
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheinv <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)      # computing inverse of matrix
        x$setinv(m)                # stores the matrix (inverse)
        m                          # returns the matrix (inverse)
}


# tested using following commands in R console:
# a <- matrix(1:4,2,2)
# b <- makeMatrix(a)
# cacheinv(b)
