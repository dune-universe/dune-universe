library(Matrix)
read_csr_file <- function(file, ncol = NULL)
{
    lines <- readLines(file)
    nrow <- length(lines)
    res <- Matrix(0, nrow, ncol)
    i <- 1
    for (line in lines) {
      cols = strsplit(line, '[ ]+')
      for (col in cols[[1]]) {
        s <- strsplit(col, ':')
        j <- as.integer(s[[1]][1])
        k <- as.numeric(s[[1]][2])
        res[i, j] <- k
      }
      i <- i + 1
    }
    res
}

file='data/small.csr'
z <- read_csr_file(file, 10)
z
