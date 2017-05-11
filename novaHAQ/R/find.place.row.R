#' Find Place Row
#'
#' Find the towns that are specified in the new coalm incorporation
#'
#' @param x The coal merchant survey data frame
#' @param del.string The string to be removed from certain variables as character vector
#' @param cleardot Logical that removes the punctuation marks from the list of places when TRUE
#' @param spacer a character vector where matches are sought, or an object which can be coerced by
#' as.character to a character vector
#' @param index Logical (not used here)
#' @param levelvar The level variable (not used here)
#' @param verbose Logical that displays function messages when TRUE
#' @param debug Logical to assign variables to the global environment for debugging purposes when TRUE
#' @export

find.place.row <- function(x = clm,
                       del.string = "^Delivery.places_",
                       cleardot = TRUE,
                       spacer = ".",
                       index = FALSE,
                       levelvar = haq$town,
                       verbose = TRUE,
                       debug = FALSE){
names.x = names(x)
placelist = gsub(del.string, "", names.x[grep(del.string, names.x)])
if (debug == TRUE) assign("placelist", placelist, envir=.GlobalEnv)
if (cleardot == TRUE) placelist = gsub("\\.", spacer, placelist)
cols = grep(del.string, names.x)
if(verbose == TRUE) message("cols: ", cols)
if (any(is.na(x[cols]))==TRUE) x[cols][which(is.na(x[cols]))] = 0 # set NA to 0
x[cols] = as.integer(x[cols])
if(verbose == TRUE) message("x[cols] ", str(x[cols]))
if(verbose == TRUE) message("which(x[cols]==1 ", x[cols]==1)
yeslist = which(x[cols]==1)
if (debug == TRUE) assign("yeslist", yeslist, envir=.GlobalEnv)
if(verbose == TRUE) message("placelist: ", placelist)
if(verbose == TRUE) message("yeslist: ", yeslist)
paste(placelist[yeslist], collapse= " ")
}


