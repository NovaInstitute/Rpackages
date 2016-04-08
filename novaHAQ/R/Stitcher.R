#' Stitcher
#'
#' Function is used to merge sheets
#'
#' @param indir The directory of the input files of concern for this function as character vector
#' @param outdir The directory to which any output files should be savesd as character vector
#' @param projectname The name of the project to be attributed to all of the desired output files
#' @param save Saves the HAQ data as an R data file in the output directory when TRUE
#' @param xlsx Creates and saves an xlsx document in the output directory when TRUE
#' @param verbose Displays function messages when TRUE
#' @param submissions creates a submissions index to go along with the selection index when TRUE
#' @param write.csv Writes a csv file of the HAQ data to the chosen output directory when TRUE
#' @param clean If TRUE acleaning function is initializes whereby certain variables are dropped from
#' the HAQ dataset
#' @export

sticher <- function(indir  = paste(dropdir,basedir,"1.ImplimentationData/rawdata/",sep=""),
                    outdir = paste(dropdir,basedir,"1.ImplimentationData/",sep=""),
                    projectname ="Seme2012",
                    save = TRUE,
                    xlsx= TRUE,
                    verbose = FALSE,
                    submissions = FALSE,
                    write.csv = FALSE,
                    clean = TRUE){

  require(xlsx)

  ###Read in files to be merged############

  i.files = list.files(indir)
  if(verbose==TRUE) message(paste(i.files, " "))
  pat ='[[:digit:]]{1,2}[[:blank:]]{1}[[:print:]]'
  select.idx = grep(pat,i.files)
  if(submissions==TRUE) {sub.idx = grep("Submissions.csv", i.files)
                         if(verbose==TRUE) message("You are inside submissions loop. sib.idx= ",sub.idx)
                         if(is.na(sub.idx) == FALSE) select.idx = c(select.idx, sub.idx)
                         if(verbose==TRUE) message("selec.idx = ",dput(select.idx))
                           }

  if(verbose==TRUE) message("select idx= ", select.idx)

  res=lapply(i.files[select.idx],FUN=function(x) read.csv(file=paste(indir,x,sep=""),stringsAsFactors=FALSE))
  if(verbose==TRUE) message("dim res= ", length(res))
  if(verbose==TRUE) assign("res",res,envir=.GlobalEnv)

  basis <- res[[1]]
  basis.lengte = nrow(basis)
  l.l = lapply(res,nrow)
  res = res[which(l.l==basis.lengte)]
  if(verbose==TRUE) message("dim basis= ", length(basis)[[1]])
  for(i in 2:length(res)){
    names(basis) = gsub("^X\\.", "", names(basis))
    names(basis) = gsub("\\.$", "", names(basis))
    names(res[[i]]) = gsub("^X\\.", "", names(res[[i]]))
    names(res[[i]]) = gsub("\\.$", "", names(res[[i]]))
    basis = merge(basis,res[[i]],
                 by.x= c("Submission.Id","Fieldworker.Id", "Fieldworker.Name"),
                 by.y= c("Submission.Id","Fieldworker.Id", "Fieldworker.Name")
                 )
  basis
      }

  if(clean == TRUE) {
    message("cleaning \n")
    dropnames = c("Fieldworker.Id", "Repeats.On.Question.x", "Repeat.Question.Value.x",
                      "Repeating.Index.x", "Received.Date.x", "Repeats.On.Question.y",
                      "Repeat.Question.Value.y", "Repeating.Index.y", "Received.Date.y",
                      "Respondent.name", "Repeats.On.Question", "Repeat.Question.Value",
                      "Repeating.Index", "Received.Date", "Device", "Received", "Start",
                      "End", "Duration..seconds", "Language", "Survey.Version")
    dropidx = names(basis) %in% dropnames
    basis = basis[, -dropidx]
    #if(verbose==TRUE) message("basis is ", dim(basis)[1] , " by ", dim(basis)[2], "\n", paste(names(basis), " "))
  }
  haq <- basis
    if(verbose==TRUE) message("haq is ", dim(haq)[1], " by ", dim(haq)[2], "\n", paste(names(haq), " "))
  haq$Location_0 = paste(haq$Longitude, haq$Latitude, sep=", ")
  if(verbose==TRUE) message("dim(haq)= ", dim(haq)[[1]] , " x ", dim(haq)[[2]])
  assign(projectname,haq,envir=.GlobalEnv)
  if(save==TRUE) save(haq,file=paste(outdir,projectname,".Rda",sep=""))
  if(write.csv==TRUE) write.csv(haq,file=paste(outdir,projectname,".csv",sep=""))
  if(xlsx==TRUE) write.xlsx2(haq,file=paste(outdir,projectname,".xlsx",sep=""))
  }
