# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

tree <- function(item, depth) {
    if (depth == 0L)
        return(c(item, NA, NA))
    # it is ridiculous that this doesn't help
    next_depth <- depth - 1L
    right_item <- 2L * item
    left_item <- right_item - 1L
    return(list(item,
                tree(left_item, next_depth),
                tree(right_item, next_depth)))
}

check <- function(tree)
    if(is.na(tree[[2]][[1]])) tree[[1]] else tree[[1]] + check(tree[[2]]) - check(tree[[3]])

binarytrees <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 10L

    min_depth <- 4L
    max_depth <- max(min_depth + 2L, n)
    stretch_depth <- max_depth + 1L

    cat(sep="", "stretch tree of depth ", stretch_depth, "\t check: ",
        check(tree(0L, stretch_depth)), "\n")

    long_lived_tree <- tree(0L, max_depth)

    for (depth in seq(min_depth, max_depth, 2L)) {
        iterations <- as.integer(2^(max_depth - depth + min_depth))
        check_sum <- sum(sapply(
                1:iterations,
		function(i) check(tree(i, depth)) + check(tree(-i, depth))))
        cat(sep="", iterations * 2L, "\t trees of depth ", depth, "\t check: ",
            check_sum, "\n")
    }

    cat(sep="", "long lived tree of depth ", max_depth, "\t check: ", 
        check(long_lived_tree), "\n")
}

if (!exists("i_am_wrapper"))
    binarytrees(commandArgs(trailingOnly=TRUE))
