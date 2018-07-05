require(stringi)
require(stringdist)    
require(phonics)

#' Does probability matching on names. 
#' If method=="all" it runs all the algorithms and returns a list #' with two vectors, 
#' the first with names that matches, and the second with suggestions from the different 
#' algorithms. Also produces a graph.
#' For any other method getMatches is called and the vector returned
#' @param string name to be matched.
#' @param vector of strings with names.
#' @param double threshold matching.
#' @param string specifying which method to use.
#' @return vector(named) with inidices of which the names has a matching > threshold
#' @export
probMathcing <- function(name = NULL, 
                          strings = NULL,
                          threshold = 0.7, 
                          method = c("all", "dl", "hamming", "lcs", "meta", "qgram", "soundex"),
                         plot = TRUE) {
  
  if (method == "all"){
    methods <- c("dl", "hamming", "lcs", "meta", "qgram", "soundex")
    uniques <- unique(strings)
    
    #runs all the algorithms
    for (i in 1:length(methods)) {
      print(methods[i])
      ls <- getMatches(name, strings, threshold, methods[i])
      #print(ls)
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
    nums <- c(length(dl), length(hamming), length(lcs), length(meta), length(qgram), length(soundex))
    if (plot) {
      plotNums(nums, methods)
    }
    
    #adds hamming & lcs to dl
    merge <- dl
    addons <- c(hamming, lcs)
    for (i in addons) {
    diffidx <- (!(i %in% dl))
     if (length(i[diffidx]) > 0) {
     merge <- append(merge, i[diffidx], after=length(merge))
     }
    }
    #add the suggestions from metaphone, q-gram and soundex
    suggest <- c()
    suggestions <- c(meta, qgram, soundex)
    for (i in suggestions) {
      diffidx <- (!(i %in% dl))
      if (length(i[diffidx]) > 0) {
        temp <- i[diffidx]
        suggest <- append(suggest, temp, after=(length(suggest)))
      }
    }
    suggest <- unique(suggest)
    all <- list(merge, suggest)
    #uncomment if a single vector is needed
    #all <- append(dl, suggest, after=length(dl))
    return(all)
  } else {
   return(getMatches(name, strings, threshold, method))
  }
}


#' Does probability matching on names 
#' @param string name to be matched.
#' @param vector of strings with names.
#' @param double threshold matching.
#' @param string specifying which method to use.
#' @return vector(named) with inidices of which the names has a matching > threshold
#' @export
getMatches <- function(name, strings, threshold, method) {
  if (method == "soundex") {
    ls <- which(stringdist(strings, name, method)==0)
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

#' Plots the matches found for the number of matches found for every name by each algorithm
#' @param list containing vectors with the data
#' @return the graph
#' @export 
plotNums <- function(nums, methods) {
  col <- c("blue", "brown", "purple", "green", "red", "yellow")
  plot.new()
  bp <- barplot(height = nums, legend.text	= nums, col = col,  xlim=c(0, length(methods)),
                width = 0.75, space = 0.25, xlab = "Methods", ylab = "Frequency", 
       main = "Matches", names.arg = methods, 
       args.legend = list(x=length(methods)+0.5, y=max(nums)), bty = "n")
  bp
  return(bp)
}



#' Creates a dataframe containing the numbers of matches made by each algorithm
#' @param vector of names to be matched.
#' @param vector of strings with names.
#' @param double threshold matching.
#' @return data.frame
#' @export
probStats <-  function(names = NULL, 
                       strings = NULL,
                       threshold = 0.7,
                       plot = TRUE) { 
  
  uniques <- unique(names)
  dfStats <- data.frame(names<-uniques)
  nums <- c()
  
  for (i in 1:length(uniques)) {
    nums[i] <- length(which(strings == uniques[i]))
  }
  dfStats$freq <- nums  
  methods <- c("dl", "hamming", "lcs", "meta", "qgram", "soundex")
  for (i in 1:length(methods)) {
    #print(methods[i])
    list <- c()
    for (j in 1:length(uniques)) {
      ls <- getMatches(uniques[j], strings, 0.7, methods[i])
      #print(ls)
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
  algs <- list(sorted$dl, sorted$hamming, sorted$lcs, sorted$qgram, sorted$meta, sorted$soundex)
  if (plot) {
    plotFreq(algs, methods)
  }
  return(sorted)
}

#' Plots the matches found forthe number of matches found for every name by each algorithm
#' @param list containing vectors with the data
#' @return the graph
#' @export 
plotFreq <- function(algs, methods) {
  x <- c(1:length(algs[[1]]))
  col <- c("blue", "brown", "purple", "green", "red", "yellow")
  p <- plot.new()
  p <- p + plot(algs[[length(algs)]], type = "l", col = col[6], xlab = "Indices", ylab = "Frequency", 
       main = "Names")
  for (i in (length(algs)-1):1) {
    p <- p + lines(x, algs[[i]], type = "l", col = col[i])
  }
  legend("topleft", inset=.05, title="Algorithms",
         methods, col=col, lwd = 2)
  p
  return(p)
}
