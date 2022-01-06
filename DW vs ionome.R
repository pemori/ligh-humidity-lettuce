library(readxl)
luces <- read_excel("G:/Mi unidad/Tesis doctorado/Ensayo luces/dataset.xlsx")
head(luces)
names <- c('dw','Cu','Zn','As','N','P','K','Ca','Mg','CE','Cl')
luces[,names] <- lapply(luces[,names] , as.numeric)
names <- c('Site','trt','light','hum')
luces[,names] <- lapply(luces[,names] , as.factor)
data <- as.data.frame(luces)

#For Cu
# scatter plot of x and y variables
library(ggplot2)
# color by groups
scatterPlot <- ggplot(data,aes(Cu, dw, color=Site)) + 
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(data, aes(Cu, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(data, aes(dw, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
ydensity

blankPlot <- ggplot()+geom_blank(aes(1,1))+
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )

library(gridExtra)
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#N
# color by groups
scatterPlot <- ggplot(data,aes(N, dw, color=Site)) + 
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(data, aes(N, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(data, aes(dw, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
ydensity

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#P
# color by groups
scatterPlot <- ggplot(data,aes(P, dw, color=Site)) + 
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(data, aes(P, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(data, aes(dw, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
ydensity

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#K
# color by groups
scatterPlot <- ggplot(data,aes(K, dw, color=Site)) + 
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(data, aes(K, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(data, aes(dw, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
ydensity

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#Ca
# color by groups
scatterPlot <- ggplot(data,aes(Ca, dw, color=Site)) + 
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(data, aes(Ca, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(data, aes(dw, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
ydensity

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#Mg
# color by groups
scatterPlot <- ggplot(data,aes(Mg, dw, color=Site)) + 
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(data, aes(Mg, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(data, aes(dw, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
ydensity

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#Zn
# color by groups
scatterPlot <- ggplot(data,aes(Zn, dw, color=Site)) + 
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(data, aes(Zn, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(data, aes(dw, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
ydensity

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

#As
# color by groups
scatterPlot <- ggplot(data,aes(As, dw, color=Site)) + 
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
scatterPlot
# Marginal density plot of x (top panel)
xdensity <- ggplot(data, aes(As, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
xdensity
# Marginal density plot of y (right panel)
ydensity <- ggplot(data, aes(dw, fill=Site)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme(legend.position = "none")
ydensity

grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, 
             ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))

