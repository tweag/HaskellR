# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------
width = 60L
lookup_size = 4096L
lookup_scale = as.double(lookup_size - 1L)

alu = paste(
    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
    "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA",
    "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT",
    "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA",
    "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
    "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
    "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA",
    sep="", collapse="")

iub = matrix(c(
    c(0.27, 'a'),
    c(0.12, 'c'),
    c(0.12, 'g'),
    c(0.27, 't'),
    c(0.02, 'B'),
    c(0.02, 'D'),
    c(0.02, 'H'),
    c(0.02, 'K'),
    c(0.02, 'M'),
    c(0.02, 'N'),
    c(0.02, 'R'),
    c(0.02, 'S'),
    c(0.02, 'V'),
    c(0.02, 'W'),
    c(0.02, 'Y')
), 2)

homosapiens = matrix(c(
    c(0.3029549426680, 'a'),
    c(0.1979883004921, 'c'),
    c(0.1975473066391, 'g'),
    c(0.3015094502008, 't')
), 2)

random <- 42L
random_next_lookup <- function() {
    random <<- (random * 3877L + 29573L) %% 139968L
    return(random * (lookup_scale / 139968))  # TODO
}

repeat_fasta <- function(s, count) {
    chars = strsplit(s, split="")[[1]]
    len = nchar(s)
    s2 = c(chars, chars[1:width])
    pos <- 1L
    while (count) {
	line = min(width, count)
        next_pos <- pos + line
        cat(s2[pos:(next_pos - 1)], "\n", sep="")
        pos <- next_pos
        if (pos > len) pos <- pos - len
	count <- count - line
    }
}

random_fasta <- function(genelist, count) {
    n = ncol(genelist)
    lookup <- integer(lookup_size)
    cprob_lookup <- cumsum(genelist[1, ]) * lookup_scale
    cprob_lookup[[n]] <- lookup_size - 1

    j <- 1L
    for (i in 1:lookup_size) {
        while (cprob_lookup[[j]] + 1L < i)
            j <- j + 1L
        lookup[[i]] <- j
    }

    while (count) {
	line <- min(width, count)
        
        rs <- double(line)
        for (i in 1:line)
          rs[[i]] <- random_next_lookup()

        inds <- lookup[rs + 1L]
        missed <- which(cprob_lookup[inds] < rs)
        if (length(missed))
            repeat {
                inds[missed] <- inds[missed] + 1L
                missed <- which(cprob_lookup[inds] < rs)
                if (!length(missed))
                    break
            }

        cat(paste(genelist[2, inds], collapse="", sep=""), "\n", sep="")
	count <- count - line
    }

}

fastaredux <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 1000L
    cat(">ONE Homo sapiens alu\n")
    repeat_fasta(alu, 2 * n)
    cat(">TWO IUB ambiguity codes\n")
    random_fasta(iub, 3L * n)
    cat(">THREE Homo sapiens frequency\n")
    random_fasta(homosapiens, 5L * n)
}

if (!exists("i_am_wrapper"))
    fastaredux(commandArgs(trailingOnly=TRUE))
