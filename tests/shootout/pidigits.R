
# Constructors and string conversion functions

zero_mag = integer(0)
zero = c(0L, zero_mag)  # ( = 0L )
one_mag = c(1L)
one = c(1L, one_mag)
ten_mag = c(10L)
ten = c(1L, ten_mag)

elem_max = 10000
elem_digits = as.integer(log10(elem_max))

signum <- function(v) v[[1]]
mag <- function(v) v[2:length(v)]

str_to_mag <- function(s) {
    strip_leading_zeros <- function(s) {
        for (i in 1:nchar(s))
            if (substr(s, i, i) != '0')
                return(substr(s, i, nchar(s)))
        return("")
    }

    len = nchar(s <- strip_leading_zeros(s))
    if (len == 0)
        return(zero_mag)
    mod <- len %% elem_digits
    if (mod > 0) chunks <- substr(s, 1, mod)
    else chunks <- character(0)
    if (1 + mod < len) {
        chunk_inds_begin <- seq(1 + mod, len, elem_digits)
        chunk_inds_end <- chunk_inds_begin + elem_digits - 1
        chunks <- c(chunks, substring(s, chunk_inds_begin, chunk_inds_end))
    }
    return(sapply(chunks, as.integer))
}
hi =
bigint <- function(s, i=NA) {
    if (!is.na(i)) {
        s <- as.character(i)
        #x <- abs(i)
        #sgn <- if (i < 0L) -1L else 1L
        #while (i >= max_elem) {
        #}
     }
#    } else {
        if (substr(s, 1, 1) == '-') {
            mag <- str_to_mag(substr(s, 2L, nchar(s)))
            sgn <- -1L
        } else {
            mag <- str_to_mag(s)
            sgn <- 1L
        }
#    }
    return(c(if (length(mag)) sgn else 0L, mag))
}

to_int <- function(x) {
    ret <- 0L
    if ((x_len = length(x)) > 1L)
        for (i in length(x):2)
            ret <- ret * elem_max + x[[i]]
    return(x[[1]] * ret)
}

mag_to_str <- function(x) {
    len = length(x)
    if (len == 1) return(as.character(x[[1]]))
    xs <- sapply(x[2:length(x)], function(e) zeropad(e, 4))
    x1 <- x[[1]]
    return(paste(collapse="", sep="", c(
                x[[1]],
                sapply(x[2:length(x)], function(e) zeropad(e, elem_digits)))))
}

to_str <- function(x) {
    if (x[[1]] == 0L)
        return("0")
    return(paste(sep="", if (x[[1]] < 0L) '-' else '', mag_to_str(mag(x))))
}

# Comparison functions

cmp_elem <- function(x, y) (x > y) - (x < y)
cmp_mag <- function(x, y) {
    x_len = length(x)
    y_len = length(y)
    if (x_len < y_len) return(-1L)
    if (x_len > y_len) return(1L)
    for (i in 1:x_len)
        if (c <- cmp_elem(x[[i]], y[[i]]))
	    return(c)
    return(0L)
}
cmp <- function(x, y) {
    x_sign = x[[1]]
    y_sign = y[[1]]
    if (x_sign == y_sign) {
        if (x[[1]] == 1L) return(cmp_mag(mag(x), mag(y)))
        if (x[[1]] == -1L) return(cmp_mag(mag(y), mag(x)))
        return(0L)
    }
    return((x_sign > y_sign) - (x_sign < y_sign))
}

eq <- function(x, y) cmp(x, y) == 0L
ne <- function(x, y) cmp(x, y) != 0L
le <- function(x, y) cmp(x, y) <= 0L
lt <- function(x, y) cmp(x, y) < 0L
ge <- function(x, y) cmp(x, y) >= 0L
gt <- function(x, y) cmp(x, y) > 0L

# Arithmetic operations

add_mag <- function(x, y) {
    # if x is shorter, swap the two vectors
    if (length(x) < length(y)) {
        tmp <- x; x <- y; y <- tmp
    }
    x_len = length(x)
    y_len = length(y)

    x_index <- x_len
    y_index <- y_len
    result <- integer(x_len)
    sum <- 0L

    # add common parts of both numbers
    while (y_index > 0L) {
        sum <- (x[[x_index]] %% elem_max + y[[y_index]] %% elem_max +
                sum %/% elem_max)  # TODO shift
        result[[x_index]] <- sum %% elem_max
	x_index <- x_index - 1L
        y_index <- y_index - 1L
    }

    # copy remainder of the longer number while carry propagation is required
    carry <- (sum >= elem_max)
    while (x_index > 0L & carry) {
        carry <- (result[[x_index]] <- x[[x_index]] + 1L) == 0L
        x_index <- x_index - 1L
    }

    # copy remainder of the longer number
    while (x_index > 0L) {
        result[[x_index]] <- x[[x_index]]
        x_index <- x_index - 1L
    }

    # grow result if necessary
    if (carry)
        return(c(0x01L, result))
    return(result)
}

add <- function(x, y) {
    if (y[[1]] == 0L)
        return(x)
    if (x[[1]] == 0L)
        return(y)
    if (x[[1]] == y[[1]])
        return(c(x[[1]], add_mag(mag(x), mag(y))))

    c <- cmp_mag(mag(x), mag(y))
    if (c == 0L)
        return(zero)
    if (c > 0L)
        result_mag <- sub_mag(mag(x), mag(y))
    else
        result_mag <- sub_mag(mag(y), mag(x))
    return(c(sign_prod(c, x[[1]]),
             strip_leading_zero_elems(result_mag)))
}

sub_mag <- function(big, little) {
    big_len = length(big)
    little_len = length(little)
    result <- integer(big_len)
    big_index <- big_len
    little_index <- little_len
    difference <- 0L

    # subtract common parts of both numbers
    while (little_index > 0L) {
        difference <- (big[[big_index]] - little[[little_index]] +
                       if (difference < 0L) -1L else 0L)
        result[[big_index]] <- difference %% elem_max
        big_index <- big_index - 1L
        little_index <- little_index - 1L
    }

    # subtract remainder of longer number while borrow propagates
    borrow <- if (difference < 0L) -1 else 0L
    while (big_index > 0L && borrow) {
        borrow <- (result[[big_index]] <- big[[big_index]] - 1L) == -1L
        big_index <- big_index - 1L
    }

    # copy remainder of the longer number
    while (big_index > 0L) {
        result[[big_index]] <- big[[big_index]]
        big_index <- big_index - 1L
    }

    return(result)
}

sub <- function(x, y) {
    if (y[[1]] == 0L)
        return(x)
    if (x[[1]] == 0L)
        return(negate(y))
    if (x[[1]] != y[[1]])
        return(c(x[[1]], add_mag(mag(x), mag(y))))

    c <- cmp_mag(mag(x), mag(y))
    if (c == 0L)
        return(zero)
    if (c > 0L)
        result_mag <- sub_mag(mag(x), mag(y))
    else
        result_mag <- sub_mag(mag(y), mag(x))
    return(c(sign_prod(c, x[[1]]),
             strip_leading_zero_elems(result_mag)))
}

negate <- function(x) c(-x[[1]], x[2:length(x)])

multiply_mag <- function(x, y) {
    x_len = length(x)
    y_len = length(y)
    x_start = x_len  # remove
    y_start = y_len  # remove
    c <- integer(x_len + y_len)

    carry <- 0L
    k <- y_start + x_start
    for (j in seq(y_start, 1L, -1L)) {
        product = y[[j]] * x[[x_start]] + carry
        c[k] <- product %% elem_max
        carry <- product %/% elem_max
        k <- k - 1L
    }
    c[x_start] <- carry

    if (x_len <= 1)
        return(c)

    for (i in seq(x_start - 1L, 1L, -1L)) {
        carry <- 0
        k <- y_start + i
        for (j in seq(y_start, 1L, -1L)) {
            product = y[[j]] * x[[i]] + c[[k]] + carry
            c[[k]] <- product %% elem_max
            carry <- product %/% elem_max
            k <- k - 1L
        }
        c[[i]] <- carry %% elem_max
    }
    return(c)
}

mul <- function(x, y) {
    if (y[[1]] == 0L || x[[1]] == 0)
        return(zero)

    return(c(sign_prod(x[[1]], y[[1]]),
            strip_leading_zero_elems(multiply_mag(mag(x), mag(y)))))
}

div_mag <- function(x_mag, y_mag) {
    if (length(y_mag) == 1L && y_mag == one_mag)
        return(x_mag)

    x <- c(1L, x_mag)
    y <- c(1L, y_mag)
    x_mag_log10 = log10_mag(x_mag); y_mag_log10 = log10_mag(y_mag)
    lo_log10 = x_mag_log10 - y_mag_log10 - (x_mag_log10 != y_mag_log10)
    hi_log10 <- x_mag_log10 - y_mag_log10 + 1L
    lo <- bigint_pow10(lo_log10)

    # try pruning hi > 10 or lo <= 10
    if (lo_log10 == 0L && hi_log10 > 1L) {
        lo10 = mul(lo, ten)
        if (gt(mul(lo10, y), x))
            hi <- lo10
        else {
            lo <- lo10
            hi <- bigint_pow10(hi_log10)
        }
    } else
      hi <- bigint_pow10(hi_log10)

    while (lt(lo, hi)) {
        mid <- div2(add(add(lo, hi), one))
        if (le(mul(mid, y), x))
            lo <- mid
        else
            hi <- sub(mid, one)
    }
    return(mag(lo))
}

div <- function(x, y) {
    # check if division by zero
    if (x[[1]] == 0L)
        return(zero)
    c <- cmp_mag(mag(x), mag(y))
    if (c == 0L)
        return(one)
    if (c < 0L)
        return(zero)
    return(c(sign_prod(x[[1]], y[[1]]), div_mag(mag(x), mag(y))))
}

# Helper arithmetic functions

div2_mag <- function(x) {
    x_len <- length(x)
    if (x_len == 1L)
        return(x %/% 2L)

    borrow <- (x[[1]] == 1)
    x_start <- borrow + 1L
    x_end <- x_len
    result_index <- 1L
    result <- integer(x_end - x_start + 1L)
    for (x_index in x_start:x_end) {
        d = x[[x_index]] + elem_max * borrow
        result[[result_index]] <- d %/% 2
        borrow <- d %% 2
        result_index <- result_index + 1L
    }
    return(result)
}

div2 <- function(x) {
    if (x[[1]] == 0L || (length(x) == 2L && x[[2]] == 1L))
        return(zero)
    return(c(x[[1]], div2_mag(mag(x))))
}

log10_mag <- function(m) elem_digits * (length(m) - 1L) + as.integer(log(m[[1]], 10))

bigint_pow10 <- function(n) c(1L, as.integer(10^(n %% elem_digits)), rep.int(0L, n %/% elem_digits))

# Misc functions
sign_prod <- function(x, y) (x == y) - (x != y)
strip_leading_zero_elems <- function(x) {
    for (i in 1:length(x))
        if (x[[i]] != 0L)
            return(x[i:length(x)])
    return(zero_mag)
}

zeropad <- function(s, n)
    paste(sep="", paste(collapse="", rep('0', max(0L, n - nchar(s)))), s)


# PIDIGITS program

pidigits <- function(args) {
    N = if (length(args)) as.integer(args[[1]]) else 100L
    ONE = one
    TWO = bigint("2")
    TEN = bigint("10")
    THREE = add(ONE, TWO)
    i <- k <- ns <- 0L
    k1 <- 1L
    a <- t <- u <- bigint("0")
    n <- d <- bigint("1")
    while (TRUE) {
        k <- k + 1L
        t <- mul(n, TWO)
        n <- mul(n, bigint(k))
        a <- add(a, t)
        k1 <- k1 + 2L
        k1_big <- bigint(i=k1)
        a <- mul(a, k1_big)
        d <- mul(d, k1_big)
        if (ge(a, n)) {
            n3a <- add(mul(n, THREE), a)
            t <- div(n3a, d)
	    td = mul(t, d)
            u <- add(sub(n3a, td), n)
            if (gt(d, u)) {
                ns <- ns * 10L + to_int(t)
                i <- i + 1L
                if (i %% 5L == 0L) {
		    cat(zeropad(as.character(ns), 5))
		    if (i %% 2L == 0L)
                       cat(sep="", "\t:", i, "\n")
                    ns = 0L
                }
                if (i >= N)
                    break
                a <- sub(a, td)  # TODO use td
                a <- mul(a, TEN)
                n <- mul(n, TEN)
            }
        }
    }
}

if (!exists("i_am_wrapper"))
    pidigits(commandArgs(trailingOnly=TRUE))
