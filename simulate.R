# This script can be used to test the calculation through the simulation

library(Biostrings)

len <- 100
n <- 10000
freqs <- c(0.2950,0.2050,0.2050,0.2950)
motif <- "SASTWB"

gen_str <- function()
  DNAString(paste0(
    sample(c('A','C','G','T'),
           size = len,
           replace = T,
           prob = freqs),
    collapse=""))
has_motif <- replicate(n, {
  str <- gen_str()
  countPattern(motif, str, fixed=F) + countPattern(motif, reverseComplement(str), fixed=F) > 0
  })
mean(has_motif)
sd(has_motif)/sqrt(length(has_motif))
