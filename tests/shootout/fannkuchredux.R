# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

fannkuch <- function(n) {
    one_two = c(1, 2)
    two_one = c(2, 1)
    two_three = c(2, 3)
    three_two = c(3, 2)
    if (n > 3L)
        rxrange = 3:(n - 1)
    else
        rxrange = integer(0)

    max_flip_count <- 0L
    perm_sign <- TRUE
    checksum <- 0L
    perm1 <- 1:n
    count <- 0:(n - 1L)
    while (TRUE) {
        if (k <- perm1[[1L]]) {
            perm <- perm1
            flip_count <- 1L
            while ((kk <- perm[[k]]) > 1L) {
                k_range = 1:k
                perm[k_range] <- rev.default(perm[k_range])
                flip_count <- flip_count + 1L
                k <- kk
                kk <- perm[[kk]]
            }
            max_flip_count <- max(max_flip_count, flip_count)
            checksum <- checksum + if (perm_sign) flip_count else -flip_count
        }

        # Use incremental change to generate another permutation
        if (perm_sign) {
            perm1[one_two] <- perm1[two_one]
            perm_sign = FALSE
        } else {
            perm1[two_three] <- perm1[three_two]
            perm_sign = TRUE
            was_break <- FALSE
            for (r in rxrange) {
                if (count[[r]]) {
                    was_break <- TRUE
                    break
                }
                count[[r]] <- r - 1L
                perm0 <- perm1[[1L]]
                perm1[1:r] <- perm1[2:(r + 1L)]
                perm1[[r + 1L]] <- perm0
            }
            if (!was_break) {
                r <- n
                if (!count[[r]]) {
                    cat(checksum, "\n", sep="")
                    return(max_flip_count)
                }
            }
            count[[r]] <- count[[r]] - 1L
        }
    }
}

fannkuchredux <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 12L
    cat("Pfannkuchen(", n, ") = ", fannkuch(n), "\n", sep="")
}

if (!exists("i_am_wrapper"))
    fannkuchredux(commandArgs(trailingOnly=TRUE))
