go <- function(e) {
  ty <- typeof(e)
  if (ty %in% c("language", "expression")) {
    lapply(e, go)
  }
  else if (ty == "symbol") {
    as.character(e)
  }
  else
    character(0)
}

cat(grep(pattern = "_hs$", value = TRUE,
    unique(unlist(recursive = TRUE,
        go(parse(file = input_file))))))
