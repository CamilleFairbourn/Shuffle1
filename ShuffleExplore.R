shuffle <- function(total1, total2, totyes){
  vec1<- c(rep("C1", total1),rep("C2", total2))
  samp1 <- sample(vec1, totyes, replace = FALSE)
  res1 <- table(samp1)
  prop1 <- c(res1[1]/total1, res1[2]/total2)
  diff1 <- prop1[1]-prop1[2]
  return(diff1)
}
library(ggplot2)
library(gridExtra)
require(BHH2)
props <- replicate(n = 50, shuffle(43,46,12))
jitprop <- jitter(props)
dotPlot(jitprop)
abline(v=0.189, col = "red")
hist(props, breaks =)
View(props)
head(props)
df <- data.frame(props)
df
ggplot(df, aes(x = props))+
  geom_dotplot()




#Code to print a table as a graphic object
mydata <- data.frame(a=1:50, b=rnorm(50))
mytable <- cbind(sites=c("site 1","site 2","site 3","site 4"),mydata[10:13,])
mymatrix <- data.frame(mytable)
# --- Graph 1 : If you want ONLY the table in your image :
# First I create an empty graph with absolutely nothing :
qplot(mymatrix, geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
  # Then I add my table :
  annotation_custom(grob = tableGrob(mymatrix))


# --- Graph 2 : If you want a graph AND a table on it :
my_plot <- ggplot(mydata,aes(x=a,y=b)) + geom_point(colour="blue") +   geom_point(data=mydata[10:13, ], aes(x=a, y=b), colour="red", size=5) + 
  annotation_custom(tableGrob(mytable), xmin=35, xmax=50, ymin=-2.5, ymax=-1)
my_plot

#Create the promotion/gender contingency table
myvector <- c(21,3,24,14,10,24,35,13,48)
mymatrix <- matrix(myvector, nrow = 3, ncol = 3, dimnames = list(c("Male","Female", "Total"),
                                                                  c("Promoted", "Not Promoted", "Total")))
mydata <- data.frame(mymatrix)


qplot(mydata, geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
  # Then I add my table :
  annotation_custom(grob = tableGrob(mymatrix))
