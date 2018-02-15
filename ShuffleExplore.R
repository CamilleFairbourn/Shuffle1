shuffle <- function(total1, total2, totyes){
  vec1<- c(rep("C1", total1),rep("C2", total2))
  samp1 <- sample(vec1, totyes, replace = FALSE)
  res1 <- table(samp1)
  prop1 <- c(res1[1]/total1, res1[2]/total2)
  diff1 <- prop1[1]-prop1[2]
  return(diff1)
}


props <- replicate(n = 100, shuffle(24,24,35))
dotPlot(props)