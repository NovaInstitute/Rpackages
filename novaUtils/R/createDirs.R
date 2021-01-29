#'@description creates directories

#'@title createDirs
#'@name createDirs
#'
#'@param dirs accepts character vector, with all the paths that need to created 
#'@export



createDirs <- function(dirs) {
  
  for (d in 1:length(dirs)) {
    if (!dir.exists(dirs[d])) {
      success <- dir.create(path = dirs[d], 
                            showWarnings = TRUE, 
                            recursive = TRUE)
      if (!success) {
        warning(sprintf("ERROR: Failed to create the following directory: %s", dirs[d]))
      }
    }
  }
}