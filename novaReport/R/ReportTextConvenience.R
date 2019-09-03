#' @title isare
#' @description Besluit oor 'is' or 'are'
#' @export

isare <- function(x){ifelse(x == 1, "is", "are")}

#' @title woodcoal
#' @param df dataframe
#' @param coal Character
#' @param wood Character
#' @export

woodcoal <- function(df = dt, 
                     coal = "solid_fuel_use_coal", 
                     wood = "solid_fuel_use_wood", 
                     yesval = "yes"){
        if(nrow(df) > 1) stop("Hierdie werk net met een lyn")
        sin <- ifelse( (!is.na(df[coal]) | !is.na(df[wood])) ,
                       ifelse(df[coal] == yesval, 
                              ifelse(df[wood] == yesval, "coal and wood", "coal"), 
                              ifelse(df[wood] == yesval, "only wood", "neither wood nor coal")), 
                       "not known")
        sin
}

#' @title praat
#' @description geriefsfunksie om tbl waarde binne-in Rmd te gebruik
#' @description neem waarde van var uit d en druk dit
#' @param vr karakter. naam van 'n kolom in d
#' @param d dataraam
#' @param plak logies. Moet goed bymekaargesit word
#' @param digits heelgetal. Hoeveel syfers na desimaal
#' @param verbose logies Praat terug of nie  

praat <- function(vr = "id_household", d = dt, plak = TRUE, digits = 6, verbose = FALSE, hoofklein = TRUE, klein = FALSE){
        if (all(vr %in% colnames(dt))){
                cls <- class(dt[,vr])
                if (verbose) message("class: ", cls)
                if (cls == "numeric"){
                        d %>% select(vr) %>% distinct() %>% round(digits = digits)
                } else {
                        if (cls == "integer"){
                                if (verbose) message("Jy's in die integer opsie")
                                a <- d %>% select(vr) %>% distinct() %>% as.data.frame() 
                                getalnaam(a[,1, drop = TRUE])
                        } else {
                                if (plak){
                                        res <- d %>% select(vr) %>% distinct()
                                        if (hoofklein) res <- res %>% tolower() %>% Hmisc::capitalize() %>% paste(collapse = " ")
                                        if (klein) res <- res %>% tolower() %>% paste(collapse = " ")
                                        res
                                } else {
                                        res <- d %>% select(vr) %>% distinct()
                                        if (hoofklein) res <- res %>% tolower() %>% Hmisc::capitalize()
                                        if (klein) res <- res %>% tolower()
                                        res
                                }
                        }
                }
        }
}


#' @title getalnaam
#' @description funksie om getalle een tot tien in woorde om te sit
#' @param x forseerbaar tot heelgetal
#' @export

getalnaam <- function(x){
        x <- unique(x)
        if (!is.integer(x)) x <- as.integer(x)
        if (x > -1 & x < 11){
                getalname = c("no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
                x <- unique(getalname[x+1])
        }
        x
}

#' @title getalenkelmeer
#' @description funksie wat getalnaam, en enkel/meervoude kombineer
#' @param vr Charackter
#' @param enkel Charackter
#' @param meer Logical 
#' @param df Dataframe
#' @param ia Logical
#' @export

getalenkelmeer <- function(vr = "stand_structures_number_formal", 
                           enkel = "structure", 
                           meer = NULL, 
                           df = dt, 
                           ia = FALSE){
        if (is.null(enkel)) enkel = vr
        if (is.character(vr)) x <- unique(df[,vr])  else x <- unique(vr)
        if (is.null(meer)) meer <- paste(enkel, "s", sep = "")
        if (ia) {
                a <- isare(x) 
        } else {
                a <- ""
        }
        #message("\n\n\n\n\n\n ", vr, x)
        gsub(" $", "", sprintf("%s %s%s", getalnaam(x), ifelse(x==1, enkel, meer), paste(" ", a, sep = "")))
}
