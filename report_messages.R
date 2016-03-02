

report_messages <- function (intname, multiname, d.f, dirr, groupvar, section) {
  message("You are now inside Section Report!")
  message("intname is: ", intname, " en multiname is: ", multiname, "str(intname)" , str(intname), "\n")
  message("d.f has dim of: ", dim(d.f)[1], " by ", dim(d.f)[2])
  message("dirr is ", dirr)
  message("groupvar: ", groupvar)
  message("intname: ", intname)
  message("multiname: ", multiname)
  message("section: ", section)
  message(paste(names(d.f), " "))
}