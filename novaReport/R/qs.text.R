#' @title qs.text
#' @description This function creates a text output from questions and returns 
#' a vector containing the question name and its text grouped together
#' @param qs Questions data frame
#' @param varname Character vector referring to the question name column
#' @param question.text Character vector referring to the question text columnn
#' @export

qs.text <- function(qs, varname="question_name", question.text="question_text"){
        qs <- as.data.frame(qs)
        tl = unlist(lapply(1:nrow(qs), function(x) sprintf(fmt = "\\item [{%s}] %s ", 
                                                           gsub("_", "\\\\_", qs[ ,varname][x]), qs[, question.text][x])))
        knitr::raw_latex(sprintf("\\begin{description} %s \\end{description}", paste(unlist(tl), collapse = " ")))
        }

#' @title qs.text.string 
#' @description This function behaves the same as qs.text, but it manipulates a character vector of format strings
#' @param qs Questions data frame
#' @param varname Character vector referring to the question name column
#' @param question.text Character vector referring to the question text columnn
#' @export

qs.text.string <- function(qs, varname="question.name", question.text="question.text"){
  tl = unlist(lapply(1:nrow(qs), function(x) sprintf("\\item [{%s}] %s ", gsub("_", "\\\\_", qs[ ,varname][x]), qs[, question.text][x])))
  cat(sprintf("\\begin{description} %s \\end{description}", paste(tl, collapse =" " )))
}

#' @title prepQsText
#' @description vat tbl_questions en berei hom voor vir gebruik deur qs.text
#' @param df tibble from tbl_questions
#' @param nm Character section_name to choose
#' @param vars Character. Columns in df to keep. Must include section_name
#' @import tidyverse
#' @export

prepQsText <- function(df, 
                       nm, 
                       vars = c("question_name", "question_text", "section_name")){
        df %>% filter(section_name == nm) %>% 
                select(vars) %>% 
                distinct() %>% 
                as.data.frame() %>% 
                mutate(question_text = map_chr(question_text, ~gsub("\\{|\\}|__|_", " ", .)))
}
