load("~/Downloads/surnames.Rda")
load("~/Downloads/names.Rda")

strings <- names
dfStats <- probStats(names = strings, strings = strings, threshold = 0.7)

diffs <- which(dfStats$dl != dfStats$hamming | dfStats$hamming != dfStats$lcs |
                 dfStats$lcs != dfStats$qgram | dfStats$qgram != dfStats$soundex | 
                 dfStats$soundex != dfStats$meta)
diffnames <- strings[diffs]

discrepancies <- dfStats[diffs,]
discrepancies <- discrepancies[order(discrepancies$dl, discrepancies$meta, discrepancies$qgram, discrepancies$soundex),]

#name <- "matsheni"
name <- "thabile"
answer <- probMathcing(name, strings, 0.7, "all")
ls <- answer[[1]]
sug <- answer[[2]]
#ls <-answer
text <- strings[ls]
names(text) <- names(ls)
print(text)
print(strings[sug])

methods <- c("dl", "hamming", "lcs", "meta", "qgram", "soundex")
for (j in discrepancies[[1]]) {
  name <- j
  print(name)
  for (i in methods) {
    print(i)
    ls <- probMathcing(name, strings, 0.7, i)
    #print(ls)
    text <- strings[ls]
    names(text) <- names(ls)
    print(text)
  }
}



