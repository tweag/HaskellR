# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

codes <- c(
    "A", "C", "G", "T", "U", "M", "R", "W", "S", "Y", "K", "V", "H", "D", "B",
    "N")
complements <- c(
    "T", "G", "C", "A", "A", "K", "Y", "W", "S", "R", "M", "B", "D", "H", "V",
    "N")
comp_map <- NULL
comp_map[codes] <- complements
comp_map[tolower(codes)] <- complements

reversecomplement <- function(args) {
    in_filename = args[[1]]
    f <- file(in_filename, "r")
    while (length(s <- readLines(f, n=1, warn=FALSE))) {
        codes <- strsplit(s, split="")[[1]]
        if (codes[[1]] == '>')
            cat(s, "\n", sep="")
        else {
            cat(paste(comp_map[codes], collapse=""), "\n", sep="")
        }
    }
    close(f)
}

if (!exists("i_am_wrapper"))
    reversecomplement(commandArgs(trailingOnly=TRUE))
