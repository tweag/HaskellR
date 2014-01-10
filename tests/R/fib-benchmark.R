fib <- function(n) {
    if(n == 0) return(1)
    if(n == 1) return(1)
    return(fib(n - 1) + fib(n - 2))
}

cat("fib in plain R:\n")
iterations <- 10
t <- system.time( replicate(iterations, fib(18)) )
t / iterations
