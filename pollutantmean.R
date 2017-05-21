pollutantmean <- function(directory, pollutant, id = 1:332)
{
  dfiles <- list.files(path=directory)
  monitors <- sapply(id,function(x) sprintf("%03d.csv",x))
  m <- c()
  
  for (mfile in dfiles[match(monitors, dfiles)])
  {
    m <- c(m,read.csv(paste(directory,mfile, sep = "/"))[[pollutant]])
  }
  mean(m[!is.na(m)])
}