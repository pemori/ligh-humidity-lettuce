library(readxl)
luces <- read_excel("G:/Mi unidad/Tesis doctorado/Ensayo luces/dataset.xlsx")
head(luces)
names <- c('dw','NP','CuP','ZnP','AsP','PP','KP','CaP','MgP','CE','Cl')
luces[,names] <- lapply(luces[,names] , as.numeric)
names <- c('sample','trt','light','hum')
luces[,names] <- lapply(luces[,names] , as.factor)
data <- as.data.frame(luces)

library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)

Ct <- data[data$Site=="Ct",]
Cp <- data[data$Site=="Cp",]

#Ct-DW
ggplot(Ct, aes(trt, dw)) +  
  geom_boxplot(aes(fill= trt), show.legend = FALSE) + 
  labs(y="Dry Weight, g")+  
  theme_bw(base_size = 22) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_fill_manual(name="PAR Intensity", 
                    values = c("Dark Gray","Light Gray","White", "Dark Gray","Light Gray","White"))

#Cp-DW
ggplot(Cp, aes(trt, dw)) +  
  geom_boxplot(aes(fill= trt), show.legend = FALSE) + 
  labs(y="Dry Weight, g")+  
  theme_bw(base_size = 22) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name="PAR Intensity", 
                    values = c("Dark Gray","Light Gray","White", "Dark Gray","Light Gray","White"))



#CE - Ct
ggplot(Ct, aes(trt, CE)) +  
  geom_boxplot(aes(fill= trt), show.legend = FALSE) + 
  labs(y=expression("gs, mmol"~m^{-2}~s^{-1}))+ 
  theme_bw(base_size = 22) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name="PAR Intensity", 
                    values = c("Dark Gray","Light Gray","White", "Dark Gray","Light Gray","White"))


#CE-Cp
ggplot(Cp, aes(trt, CE)) +  
  geom_boxplot(aes(fill= trt), show.legend = FALSE) + 
  labs(y=expression("gs, mmol"~m^{-2}~s^{-1}))+  
  theme_bw(base_size = 22) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name="PAR Intensity", 
                    values = c("Dark Gray","Light Gray","White", "Dark Gray","Light Gray","White"))

#Cl - Ct
ggplot(Ct, aes(trt, Cl)) + 
  geom_boxplot(aes(fill= trt), show.legend = FALSE) + 
  labs(y=expression("Chlorophyll, µg"~mL^{-1}))+  
  theme_bw(base_size = 22) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name="PAR Intensity", 
                    values = c("Dark Gray","Light Gray","White", "Dark Gray","Light Gray","White"))


#Cl-Cp
ggplot(Cp, aes(trt, Cl)) +  
  geom_boxplot(aes(fill= trt), show.legend = FALSE) + 
  labs(y=expression("Chlorophyll, µg"~mL^{-1}))+  
  theme_bw(base_size = 22) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name="PAR", 
                    values = c("Dark Gray","Light Gray","White", "Dark Gray","Light Gray","White"))

#CuP - Ct
ggplot(Ct, aes(trt, CuP)) +  
  geom_boxplot(aes(fill= trt), show.legend = FALSE) + 
  labs(y=expression("Plant Total Cu, mg"~kg^{-1}))+  
  theme_bw(base_size = 22) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name="PAR Intensity", 
                    values = c("Dark Gray","Light Gray","White", "Dark Gray","Light Gray","White"))


#CuP-Cp
ggplot(Cp, aes(trt, CuP)) +  
  geom_boxplot(aes(fill= trt), show.legend = FALSE) + 
  labs(y=expression("Plant Total Cu, mg"~kg^{-1}))+  
  theme_bw(base_size = 22) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name="PAR Intensity", 
                    values = c("Dark Gray","Light Gray","White", "Dark Gray","Light Gray","White"))

