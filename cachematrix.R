##makeCacheMAtrix(M) ; has 4 function get, set, setinverse, getinverse
##setinverse() is used in cacheSolve() function 
##using <<- operator in set() function AND setinverse() function

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL #initiate
        #1
        set <- function(y) { ##y is input value
                x<<-y ## x: parent is makeMatrix
                M<<-matrix() ##NULL
        }
        #2
        get <- function() x
        #3
        setinverse <- function(inverseM) { ##used in cache
                M <<- inverseM ## m : parent is makeCache...
        }
        #4
        getinverse <- function() M
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
        
}


## cacheSolve() ; using solve() function, calculate inverse of x
## check: det!=0 AND square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        M <- x$getinverse()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- x$get() #save matrix in data 
        if(nrow(data)!=ncol(data)) { ##check square
                message("ERROR:not a square matrix")
                return(M)
        }
        else if(det(data)==0) { ##check det
                message("ERROR:det == 0")
                return(M)
        }
        #inverse data
        M<- solve(data)
        x$setinverse(M)
        M
}
