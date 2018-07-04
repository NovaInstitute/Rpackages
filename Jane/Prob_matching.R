
#' Does probability matching on names
#' 
#' @param string name to be matched.
#' @param vector of strings with names.
#' @param double threshold matching.
#' @return vector(named) with inidices > threshold; names: prob matching
#' @export

require(stringi)
require(stringdist) #--> read up on https://www.joyofdata.de/blog/comparison-of-string-distance-algorithms/
require(phonics)

#delete this!
strings <- surnames
name <- "makhowane"

probMathcing <- function(name = NULL, 
                          strings = NULL,
                          threshold = 0.7, 
                          method = c("all", "dl", "hamming", "lcs", "qgram", "meta", "soundex")) {
  
  if (method == "all"){
    methods <- c("dl", "hamming", "lcs", "qgram", "meta", "soundex")
    uniques <- unique(strings)
    
    for (i in 1:length(methods)) {
      print(methods[i])
     # list <- c()
      ls <- getMatches(name, strings, threshold, methods[i])
      print(ls)
      #len <- length(ls)
     # list[j] <- len
      if (methods[i]=="dl"){
        dl <- ls
      }
      else if (methods[i]=="hamming"){
        hamming <- ls
      }
      else if (methods[i]=="lcs"){
        lcs <- ls
      }
      else if (methods[i]=="qgram"){
        qgram <- ls
      }
      else if (methods[i]=="soundex"){
        soundex <- ls
      }
      else if (methods[i]=="meta"){
        meta <- ls
      }
    }
    
    #plotFreq(dl, hamming, lcs, qgram, meta, soundex)
    merge <- c()
    addons <- c(hamming, lcs)
    for (i in addons) {
    diffidx <- (!(i %in% dl))
     if (length(i[diffidx]) > 0) {
     merge <- append(dl, i[diffidx], after=length(dl))
     }
    }
    merge <- unique(merge)
    
    suggest <- c()
    suggestions <- c(meta, qgram, soundex)
    for (i in suggestions) {
      diffidx <- (!(i %in% dl))
      if (length(i[diffidx]) > 0) {
        suggest <- i[diffidx]
        names(suggest) <- "suggestion"
      }
    }
    suggest <- unique(suggest)
    all <- append(merge, suggest, after=length(merge))
    return(dl)
    
  } else {
   return(getMatches(name, strings, threshold, method))
  }
}

getMatches <- function(name, strings, threshold, method) {
  if (method == "soundex") {
    ls <- which(stringdist(strings, name, method)==0)
    #ls_num <- length(ls)
  } else if (method == "meta") {
    met <- metaphone(name)
    meta <- metaphone(strings)
    dfNames <- data.frame(strings, meta)
    ls <- which(meta == met)
  } else {
   len <- nchar(name)
   threshold <- len * (1-threshold)
   dist <- c(stringdist(name, strings, method))
    matches <- which(dist <= threshold)
    nums <- dist[which(dist <= threshold)]
  
    ls <- c(matches)
    names(ls) <- (len-nums)/len
  }
  return(ls)
}  


name <- "makhowane"
probMathcing(name, strings, 0.7, "all")

diffs <- which(dfStats$dl != dfStats$hamming | dfStats$hamming != dfStats$lcs |
                 dfStats$lcs != dfStats$qgram | dfStats$qgram != dfStats$soundex | 
                 dfStats$soundex != dfStats$meta)
diffnames <- strings[diffs]

name <- "matsheni"
for (j in diffnames) {
  name <- j
  for (i in 1:length(methods)) {
   print(methods[i])
   ls <- probMathcing(name, strings, 0.7, methods[i])
   print(length(ls))
    print(ls)
  }
}

discrep <- dfStats[diffs,]
discrep <- discrep[order(discrep$dl, discrep$qgram, discrep$soundex, discrep$meta),]



#dfStats <- NULL
strings <- surnames
#Create dfStats
probStats <-  function(name = NULL, 
                       strings = NULL,
                       threshold = 0.7, 
                       method = c("all", "dl", "hamming", "lcs", "qgram", "meta", "soundex")) {
  
  uniques <- unique(strings)
  dfStats <- data.frame(names<-uniques)
  nums <- c()
  
  for (i in 1:length(uniques)) {
    nums[i] <- length(which(strings == uniques[i]))
  }
  dfStats$freq <- nums  
  methods <- c("dl", "hamming", "lcs", "meta", "qgram", "soundex")
  for (i in 1:length(methods)) {
    print(methods[i])
    list <- c()
    for (j in 1:length(uniques)) {
      ls <- getMatches(uniques[j], strings, 0.7, methods[i])
      print(ls)
      len <- length(ls)
      list[j] <- len
    } 
    if (methods[i]=="dl"){
      dfStats$dl <- list
    }
    else if (methods[i]=="hamming"){
      dfStats$hamming <- list
    }
    else if (methods[i]=="lcs"){
      dfStats$lcs <- list
    }
    else if (methods[i]=="qgram"){
      dfStats$qgram <- list
    }
    else if (methods[i]=="soundex"){
      dfStats$soundex <- list
    }
    else if (methods[i]=="meta"){
      dfStats$meta <- list
    }
  }
  sorted <- dfStats[order(dfStats$dl, dfStats$hamming, dfStats$lcs, dfStats$qgram, dfStats$meta, dfStats$soundex),]
  plotFreq(sorted$dl, sorted$hamming, sorted$lcs, sorted$qgram, sorted$meta, sorted$soundex)
}

  
  
plotFreq <- function(dl, hamming, lcs, qgram, meta, soundex) {
  x <- c(1:length(dl))
  col <- c("blue", "brown", "purple", "green", "red", "yellow")
  plot.new()
  plot(soundex, type = "l",col = col[6], xlab = "Indices", ylab = "Frequency", 
       main = "Names")
  lines(x, qgram, type = "l", col = col[5])
  lines(x, meta, type = "l", col = col[4])
  lines(x, lcs, type = "l", col = col[3])
  lines(x, hamming, type = "l", col = col[2])
  #lines(x, freq, type = "l", col = "green")
  lines(x, dl, type = "l", col = col[1])
  legend("topleft", inset=.05, title="Algorithms",
         methods, col=col, lty = 1)
  #lines(x, soundex, type = "l", col = "purple", alpha=0.5)
  #rgb(255, 255, 0, max = 255, alpha = 150, names = "blue50")
}
 
  # Create the data for the chart.
  x <- c(1:length(uniques))
  freq <- unlist(sorted$freq)
  dl <- unlist(sorted$dl)
  hamming <- unlist(sorted$hamming)
  lcs <- unlist(sorted$lcs)
  qgram <- unlist(sorted$qgram)
  soundex <- unlist(sorted$soundex)
  meta <- unlist(sorted$meta)
  
 
  

  
 
