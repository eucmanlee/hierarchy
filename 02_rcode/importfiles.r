impf<- function(x) {
  files <- x
  j=1
  for(i in seq(along=files)) {
    d.temp <- read.csv(files[i])
    if (j==1) {
      h.raw<-d.temp
    }
    else {
      h.raw <- rbind(h.raw,d.temp)  
    }
    j<-j+1
  }
  return(h.raw)
}