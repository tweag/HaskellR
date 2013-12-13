# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

spectralnorm <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 100L
    options(digits=10)

    eval_A <- function(i, j) 1 / ((i + j) * (i + j + 1) / 2 + i + 1)
    eval_A_times_u <- function(u) {
        ret <- double(n)
        for (i in 0:n1) {
            eval_A_col <- double(n)
            for (j in 0:n1)
	    eval_A_col[[j + 1]] <- eval_A(i, j)
            ret[[i + 1]] <- u %*% eval_A_col
        }
        return(ret)
    }
    eval_At_times_u <- function(u) {
        ret <- double(n)
        for (i in 0:n1) {
            eval_At_col <- double(n)
            for (j in 0:n1)
	    eval_At_col[[j + 1]] <- eval_A(j, i)
            ret[[i + 1]] <- u %*% eval_At_col
        }
        return(ret)
    }
    eval_AtA_times_u <- function(u) eval_At_times_u(eval_A_times_u(u))

    n1 <- n - 1
    u <- rep(1, n)
    v <- rep(0, n)
    for (itr in seq(10)) {
        v <- eval_AtA_times_u(u)
        u <- eval_AtA_times_u(v)
    }

    cat(sqrt(sum(u * v) / sum(v * v)), "\n")
}

if (!exists("i_am_wrapper"))
    spectralnorm(commandArgs(trailingOnly=TRUE))
