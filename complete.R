complete <- function(directory, id = 1:332)
{
  dfiles <- list.files(path=directory)
  monitors <- sapply(id,function(x) sprintf("%03d.csv",x))
  ccr <- data.frame(id=integer(), nobs=integer())

  for (mfile in dfiles[match(monitors, dfiles)])
  {
    f <- read.csv(paste(directory,mfile, sep = "/"))
    cc <- complete.cases(f)
    ccr <- rbind(ccr, c(f[1,4],length(cc[cc==TRUE])))
  }

  colnames(ccr)<-c("id","nobs")
  ccr
}