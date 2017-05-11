#' Cleaner
#'
#' Function creates a vector of clean (i.e. remove letter at end) stand numbers
#'
#' @param df Data frame containing survey data
#' @param mainplace_name The variable containing the main place names as character vector
#' @param standvar The variable containing stand numbers as character vector
#' @param surnamevar The variable containing surnames as character vector
#' @examples ermelo=cleannr(ermelo,"mainplace_name","standnumber","surname")
#' @export

cleannr=function(df,mainplace_name="mainplace_name",standvar="standnumber",surnamevar="surname"){
df$cleannr=splitnr(df[,paste(standvar)])
df$hh.ID=paste(df[,paste(mainplace_name)],df$cleannr,df[,paste(surnamevar)],sep="")
df=df[which(duplicated(df$hh.ID)==FALSE),]
df
}

#' Base Number
#'
#' Find original stand of subdivided stands write with / or .
#'
#' @param x A character vector where matches are sought, or an
#' object which can be coerced by as.character to a character vector. Long vectors are supported
#' @export

base.number=function(x){
 	patt='(^[[:digit:]]+)(\\/?)(\\.?)(\\-?)([[:alnum:]]*)'
 	gsub(patt,'\\1',x)
}

#' Isolate Number
#'
#' Isolate only the stand number
#'
#' @param x A character vector where matches are sought, or an
#' object which can be coerced by as.character to a character vector. Long vectors are supported
#' @export

isolate.number=function(x){
	patt <- '(^[[:alpha:]]*)(\\/?)(\\.?)(\\-?)([[:blank:]]*)([[:digit:]]+)([[:blank:]]*)([[:punct:]]*)([[:alpha:]]*)([[:blank:]]*)([[:punct:]]*$)'
	gsub(patt,'\\6',x)
}

#' Isolate Name
#'
#' Isolate only the name associated with a stand
#'
#' @param x A character vector where matches are sought, or an
#' object which can be coerced by as.character to a character vector. Long vectors are supported
#' @export

isolate.name=function(x){
	patt <- '(^[[:alpha:]]*)(\\/?)(\\.?)(\\-?)([[:blank:]]*)([[:digit:]]+)([[:blank:]]*)([[:alpha:]]*)([[:punct:]]*)([[:alpha:]]*)([[:blank:]]*)([[:punct:]]*$)'
	gsub(patt,'\\1 \\8',x)
}

#' Base Number
#'
#' Find original stand of subdivided stands write with / or .
#'
#' @param x A character vector where matches are sought, or an
#' object which can be coerced by as.character to a character vector. Long vectors are supported
#' @export

letter.number=function(x){
 	patt='(^[[:alpha:]]{1})(\\/?)(\\.?)(\\-?)([[:blank:]]*)([[:digit:]]+)([[:blank:]]*)([[:punct:]]*)([[:alpha:]]*)([[:blank:]]*)([[:punct:]]*$)'
 	gsub(patt,'\\1\\6',x)
 	}

