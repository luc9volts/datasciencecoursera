corr <- function(directory, threshold = 0)
{
  dfiles <- list.files(path=directory)
  ret <- c()

  for (mfile in dfiles)
  {
    f <- read.csv(paste(directory,mfile, sep = "/"))
    cc <- complete.cases(f)
    l <- length(cc[cc==TRUE])
    
    if(l > threshold){
      ret <- c(ret, cor(f$nitrate, f$sulfate, use = "complete.obs"))
    }
  }
  ret
}