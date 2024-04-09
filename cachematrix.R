## ############################################################################

#As you will notice, the code of my two functions is based on the example of
#"makeVector" and "cachemean" provided in the "README.md-file" and will as such 
# demonstrate a lot of resemblance to the code in those two functions.  
# Any how, from my understanding the functions do what they are supposed to do
#according to the provided instructions within the README.md-file. Feel free to
#test them.

#Sincerely,
#Erik


######################### THE MATRIX INVERSION FUNCTIONS #######################


#1.The makeCacheMatrix function creates a special "matrix" object that can cache
#its inverse.
#Technically, it returns a list of the 4 functions within the 
#makeCacheMatrix environment and associated data to be later called a pone by
#the cacheSolve-function, effectively encapsulating the specified matrix and its
#inverse with the functions that operate on them, i.e., creating closure.

#2. The cacheSolve function computes the inverse of the matrix specified in
#makeCacheMatrix and cache it for future use. Before it does that, however,
#it checks to see if there already is an inverse matrix in the cache for the 
#matrix specified. If so, it doesn't compute inverse matrix and instead retains
#it from the cache, prompting the message "getting cached data"  

################################################################################


#The makeCacheMatrix 
# first initializes an empty cache object to store the matrix and its inverse. 
# the set function then updates the matrix and clear any previously.
#cached inverse from a previous matrix.
#get function returns the current matrix specified.
#setinverse function cache the inverse of the matrix.
#getinverse used to return the cached inverse of the matrix. 
# Returns a list of the functions within the environment used for
#setting and getting the specified matrix and its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL 
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

### cacheSolve ###
#Takes on the argument, or the object created by the makeCacheMatrix and allows
#arguments to be passed to the solve function.
#Then it retrives the the cached inverse of the matrix if the inverse is not
#null and prompts the message.
#If the cached inverse is null it instead retains the matrix and computes the 
#inverse of the matrix with any additional arguments. 
#It then sets the inverse of the matrix in the cache and returns the inverese of
#the matrix.  


cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}

######################### Test #################################################

m <- matrix(c(2, 1, 0, 0, 3, -1, 1, 2, 4), nrow = 3)
cache_Matrix <- makeCacheMatrix(m)
inverse_m <- cacheSolve(cache_Matrix)
inverse_m <- cacheSolve(cache_Matrix)
inverse_m
#[,1]        [,2]       [,3]
#[1,]  0.51851852 -0.03703704 -0.1111111
#[2,] -0.14814815  0.29629630 -0.1111111
#[3,] -0.03703704  0.07407407  0.2222222

m %*% inverse_m # identity matrix 
#     [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1
