#' @title t_test_woorde
#' @description Performs and reports on a T-test
#' @param df Dataframe
#' @param vr numeric Variable in terms of which T-test is performed 
#' @param gp character or factor Name of grouping variable. Will be coerced to a factor with two levels

t_test_woorde <- function(df, vr, gp, alpha = 0.05, short = TRUE){
  if (!is.data.frame(df)) stop("df must be a dataframe")
  if(any(is.na(match(c(vr, gp), names(df))))) stop(paste("names of df must contain ", vr,  " and ", gp))
  df[,gp] <- as.factor(as.character(df[,gp]))
  if (nlevels(df[,gp]) != 2) stop(paste(gp, "must have 2 levels"))
  frm <- as.formula(paste(vr, " ~ ", gp))
  tt <- t.test(frm, data = df)
  signot <- ifelse(tt$p.value < alpha, "differs significantly", "does not differ significanly")
  
  if (short == FALSE) {
    out <- sprintf("At the %.2f %% level of significance, the results of Student's t-test on %s by %s indicate that the %s (%.2f) %s from the %s (%.2f, p-value %.2f)." , 
                   alpha*100, 
                   vr, 
                   gp, 
                   names(tt$estimate[1]), 
                   round(tt$estimate[1],2), 
                   signot, 
                   names(tt$estimate[2]), 
                   round(tt$estimate[2],2), round(tt$p.value, 2))
  } else {
    out <- sprintf("The mean of %s %s between %s and %s.", vr, signot, levels(df[,gp])[1], levels(df[,gp])[2])
  }
  
  out
}