## fixname function: Make variable names lower case, without spaces, without trailing _0

fixname=function(data){
  data=as.character(data)
  data=tolower(data)
  data=gsub(" ",".",data)
  data=gsub("/",".",data)
  data=gsub("-",".",data)
  data=gsub("_0","",data)
  data=gsub("\\.0","",data)
  data=gsub("\\?","",data)
  data
}
