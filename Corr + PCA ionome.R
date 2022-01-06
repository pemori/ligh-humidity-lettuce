library(readxl)
luces <- read_excel("G:/Mi unidad/Tesis doctorado/Ensayo luces/dataset.xlsx")
head(luces)
names <- c('dw','Cu','Zn','As','N','P','K','Ca','Mg','CE','Cl')
luces[,names] <- lapply(luces[,names] , as.numeric)
names <- c('Site','trt','light','hum')
luces[,names] <- lapply(luces[,names] , as.factor)
data <- as.data.frame(luces)

library(factoextra)
library(FactoMineR)
library(dplyr)
datapca<-data %>%
  dplyr::select(CE,Cl,N,P,K,Ca,Mg,Cu,Zn,As) 

library(ggplot2)
library(gclus)

data.cor <- cor(datapca)
# assign color
data.color <- dmat.color(data.cor)

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); 
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r = ", txt, sep = "")
  
  # try to set background here, but it doesn't affect the output
  if (r > 0.5) 
    par(bg = "white")
  
  text(0.5, 0.6, txt)
  
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p = ", txt2, sep = "")
  if (p < 0.01) txt2 <- "p < 0.01"
  text(0.5, 0.4, txt2)
}

cpairs(datapca,border.color="black",upper.panel=panel.cor)

library("PerformanceAnalytics")
chart.Correlation(datapca, histogram=TRUE, pch=19)

#PCA
data.pca<-PCA(datapca, scale.unit = TRUE) #PCA from FactoMineR
str(datapca)
str(data.pca) 

eigenvalues <- data.pca$eig
head(eigenvalues[, 1:2])

barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="black")

lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

head(data.pca$var$coord)
head(data.pca$var$cos2) 
head(data.pca$var$contrib) 
plot(data.pca, choix = "var", )

fviz_pca_var(data.pca, col.var="black")+ theme(
  panel.background = element_rect(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  axis.text = element_blank(), text = element_text(size=15)
  )

pc<-princomp(na.omit(datapca), cor = TRUE,scores=TRUE) 
summary(pc)
pc$loadings[,1:3]
pc$loadings
loadings(pc) 
data.pca$var$coord  


#How many components to keep? Using Kaiser criteria, I only study those variables with eigenvalue > 1.
pc$sdev^2 #eigenvalue is equal to the square of standard deviation. Thus, I keep 3 PCs.
screeplot(pc, type="lines")

# Contributions of variables to PC1 
fviz_contrib(pc, choice = "var", axes = 1, top = 10) #P,K, Ca, Zn
# Contributions of variables to PC2
fviz_contrib(pc, choice = "var", axes = 2, top = 10) #Mg, Cu, Zn
# Contributions of variables to PC3
fviz_contrib(pc, choice = "var", axes = 3, top = 10) #Mg, N, As

data.pca<-PCA(datapca) 
eig.val <- get_eigenvalue(data.pca)
eig.val
#Results for variable
var <-get_pca_var(data.pca)

# Coordinates #coordinates of variables to create a scatter plot
var$coord
  
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

data.frame(df, stringsAsFactors = TRUE)
Elem<-c('N','P','K','Ca','Mg','Cu','Zn','As')
Dim1<-c(0.5385263,0.7332571,0.7975871,-0.7014699,-0.1988976,-0.5836610,-0.6925797,0.3531289)
Dim2<-c(0.37022438,0.15489577,0.39481479,-0.29678822,-0.51964494,0.77411466,0.67771488,-0.05155387)
Dim3<-c(0.599339074,0.086895166,0.342502603,0.394504306,0.680746266,0.096715849,-0.004349719,-0.549610918)
df<-data.frame(Elem,Dim1,Dim2,Dim3)

#loadings
library(tidyverse)
ggplot(df, aes(
  x = fct_relevel(Elem,'N','P','K','Ca','Mg','Cu','Zn','As'),
  y = Dim1)) +
  geom_col()+
  xlab("Plant ionome") +
  ylab("Loadings PC-1") +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 0, colour = "black"),
         axis.text.y = element_text(hjust = 0, colour = "black"))
  
ggplot(df, aes(
  x = fct_relevel(Elem,'N','P','K','Ca','Mg','Cu','Zn','As'),
  y = Dim2)) +
  geom_col()+
  xlab("Plant ionome") +
  ylab("Loadings PC-2") +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 0, colour = "black"),
        axis.text.y = element_text(hjust = 0, colour = "black"))

ggplot(df, aes(
  x = fct_relevel(Elem,'N','P','K','Ca','Mg','Cu','Zn','As'),
  y = Dim3)) +
  geom_col()+
  xlab("Plant ionome") +
  ylab("Loadings PC-3") +
  theme_bw()+
  theme(axis.text.x = element_text(hjust = 0, colour = "black"),
        axis.text.y = element_text(hjust = 0, colour = "black"))
