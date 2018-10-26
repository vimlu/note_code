source("https://bioconductor.org/biocLite.R")
## or source("http://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
biocLite("RTCGA.clinical")
biocLite("ballgown")
install.packages("devtools")
devtools::install_github('alyssafrazee/RSkittleBrewer')



tx<-runif(100)
ty<-rnorm(100)+5*tx
tmodel<-lm(ty~tx)
attributes(tmodel)

mode(tmodel)
plot(kmfit2, col = c("red", "blue"))
points(kmfit2, col=c("red", "blue"), pch = "+")


dfNew  <- data.frame(clinic=factor(c("1", "2"), levels=levels(as.factor(addicts$clinic))),
                     X=c(-2, -2),
                     prison=factor(c("0", "1"), levels=levels(as.factor(addicts$prison))))

d=data.frame(name=c("李明","张聪","王建"),age=c(30,35,28),height=c(180,162,175))

cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file="ex.data", sep="\n")



###=====================
options(digits=2)
Student <- c("John Davis", "Angela Williams", 
             "Bullwinkle Moose", "David Jones", 
             "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English,
                     stringsAsFactors=FALSE)
z <- scale(roster[,2:4]) 
score <- apply(z, 1, mean)                                            
roster <- cbind(roster, score)
y <- quantile(score, c(.8,.6,.4,.2))                                   

roster$grade[score >= y[1]] <- "A"                                     
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"

###=================
feelings <- c("sad", "afraid")
for (i in feelings)
  print(
    switch(i,
           happy  = "I am glad you are happy",
           afraid = "There is nothing to fear",
           sad    = "Cheer up",
           angry  = "Calm down now"
    )
  )


w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5,
       66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)

X<-c(159, 280, 101, 212, 224, 379, 179, 264,
     222, 362, 168, 250, 149, 260, 485, 170)
t.test(X,mu=225,alternative="greater")




mouse<-data.frame(
  X=c( 2, 4, 3, 2, 4, 7, 7, 2, 2, 5, 4, 5, 6, 8, 5, 10, 7,
       12, 12, 6, 6, 7, 11, 6, 6, 7, 9, 5, 5, 10, 6, 3, 10),
  A=factor(c(rep(1,11),rep(2,10), rep(3,12))))
mouse.aov<-aov(X ~ A, data=mouse)
summary(mouse.aov)


##regression analysis
x<-c(0.10,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.20,0.21,0.23)
y<-c(42.0,43.5,45.0,45.5,45.0,47.5,49.0,53.0,50.0,55.0,55.0,60.0)
lm.sol<-lm(y ~ 1+x)
##先从图形上大致判断是否具有线性
plot(x,y)
abline(lm.sol)
summary(lm.sol)



## pheatmap for phynogenetic tree
require(pheatmap)
#read.table("clipboard", header = T, sep="\t")->P025
#P025[,1]->P025_rowname
#P025[,-1]->P025
#as.matrix(P025)->P025
#rownames(P025)<-P025_rowname

## read the data with first column as rowname!!
read.table("clipboard", header = T, row.names = 1, na.strings = 'NA')->P025
pheatmap(P025, color = colorRampPalette( c("white", "red", "darkred"))(200),display_numbers = T, number_format = "%.2f", cluster_rows = F, cluster_cols = F)

windowsFonts(Times=windowsFont("Times New Roman"))
par(mar=c(3,4,2,5), family="Times", ps=14)


##==========to perform chisq.test in batch============##
read.table('clipboard',header = T)->test
test$Gene->rownames(test)
test[,-1]->test
as.matrix(test)->test
for(i in 1:length(test[,1])){
  tmp_matrix<-matrix(test[i,],ncol = 2,byrow = T)
  chisq.test(tmp_matrix)$p.value->pval[i]
  # cat(pval[i])
}
names(pval)<-rownames(test)
pval[pval<0.05]->pval_cand


