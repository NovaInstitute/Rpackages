#'@title duplicates
#'@name duplicates
#'
#'@description function that identifies duplicates
#'
#'@param x [data.frame/vector] The data frame/vector containing duplicates.
#'@param vnms Names of variables to consider when checking for duplicates.
#'  Ignored when x is not a data.frame. Default is all variables in x.
#'@return duplicate rows/values
#'
#'@export

duplicates <- function(x, uniq = TRUE, vnms = NULL) {
        
        if (is.data.frame(x)) {
                if (length(vnms) == 0) { vnms <- names(x) }
        }
        
        if (uniq) {
                if (is.vector(x)) {
                        return(unique(x[which(duplicated(x))]))
                }
                if (is.data.frame(x)) {
                        x <- x[duplicated(x[,vnms]),]
                        return(x[which(!duplicated(x[,vnms])),])
                }
        }
        
        if (is.vector(x)) {
                return(x[which(x %in% duplicates(x))])
        }
        
        if (is.data.frame(x)) {
                x$xxx_in_dup_xxx <- 0
                x$xxx_in_dup_xxx[which(duplicated(x[,vnms]))] <- 1
                x <- x[nrow(x):1,]
                x$xxx_in_dup_xxx[which(duplicated(x[,vnms]))] <- 1
                x <- x[nrow(x):1,]
                x <- x[which(x$xxx_in_dup_xxx == 1),]
                x[,c("xxx_in_dup_xxx")] <- NULL
                return(x)
        }
        
        stop(sprintf("Do not know how to work with objects of class '%s'.",
                     paste(class(x), collapse = ",")))
}

