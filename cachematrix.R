## Put comments here that give an overall description of what your
## functions do

## make a 2x2 matrix "y" based on input data a, b, c, and d, and
## then calculate and output inverse of matrix "y" called "invy"
## cache matrices "y" and "invy" to "cache" and "cachei", respectively

makeCacheMatrix <- function(a,b,c,d) {
    cache <<- NULL
    cachei <<- NULL
    x=c(a,b,c,d)
    y=matrix(x,2)
    ## Calculating components of the matrix invy, which is the inverse of matrix y
    message("calculating inverse of input matrix, and add to cachei")
    detd=1/(a*d-b*c)
    e=detd*d
    f=detd*(-b)
    g=detd*(-c)
    h=detd*a
    z=c(e,f,g,h)
    invy=matrix(z,2)
    cache <<- y
    cachei <<- invy
    invy
}

## make a 2x2 matrix "v" based on input data a, b, c,and d
## check matrix "v" matches "cache" (i.e., cached matrix "y")
## if matrix "v" equal "cache", then print "cachei" (i.e., cached "invy")
## if matrix "v" not equal "cache", then calculate and output inverse of matrix "v"

cacheSolve <- function(a,b,c,d) {
    w=c(a,b,c,d)
    v=matrix(w,2)
    if((!is.null(cache)) && (identical(cache,v)==TRUE)) {
        message("getting cached data...")
        ## Return "cachei" (i.e., cached inverse of matrix "y")
        print (cachei)
    } else {
        ## Calculating components of the matrix "invv", which is the inverse of matrix "v"
        message("calculating inverse of input matrix")
        detd=1/(a*d-b*c)
        e=detd*d
        f=detd*(-b)
        g=detd*(-c)
        h=detd*a
        z=c(e,f,g,h)
        invv=matrix(z,2)
        ## Return a matrix that is the inverse of matrix "v"
        invv
    }
}
