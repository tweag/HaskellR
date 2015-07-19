# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

spectralnorm_math <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 100L
    options(digits=10)

    eval_A <- function(i, j) 1 / ((i + j - 2) * (i + j - 1) / 2 + i)

    m <- outer(seq(n), seq(n), FUN=eval_A)
    cat(sqrt(max(eigen(t(m) %*% m)$val)), "\n")
}

if (!exists("i_am_wrapper"))
    spectralnorm_math(commandArgs(trailingOnly=TRUE))
